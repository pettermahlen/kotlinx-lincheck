/*
 * Lincheck
 *
 * Copyright (C) 2019 - 2022 JetBrains s.r.o.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Lesser Public License for more details.
 *
 * You should have received a copy of the GNU General Lesser Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/lgpl-3.0.html>
 */

package org.jetbrains.kotlinx.lincheck.strategy.managed

import java.util.*

/**
 * Tracks synchronization operations with locks.
 */
interface MonitorTracker {

    fun acquire(iThread: Int, monitor: OpaqueValue): Boolean

    fun acquire(iThread: Int, monitor: OpaqueValue, times: Int): Boolean {
        require(times > 0)
        if (!acquire(iThread, monitor))
            return false
        repeat(times - 1) {
            acquire(iThread, monitor)
                // TODO: use ensure
                .also { check(it) }
        }
        return true
    }

    fun release(iThread: Int, monitor: OpaqueValue)

    fun release(iThread: Int, monitor: OpaqueValue, times: Int) {
        require(times > 0)
        require(reentranceDepth(iThread, monitor) >= times)
        repeat(times) {
            release(iThread, monitor)
        }
    }

    fun canAcquire(iThread: Int, monitor: OpaqueValue): Boolean =
        owner(monitor)?.let { it == iThread } ?: true

    fun owner(monitor: OpaqueValue): Int?

    fun reentranceDepth(iThread: Int, monitor: OpaqueValue): Int

    fun wait(iThread: Int, monitor: OpaqueValue): Boolean

    fun notify(iThread: Int, monitor: OpaqueValue, notifyAll: Boolean)

    fun isWaiting(iThread: Int): Boolean

    fun reset()

}

/**
 * Monitor tracking for model checking strategy.
 * Represents the set of acquired monitors as a map Monitor -> ThreadId.
 */
class MapMonitorTracker(val nThreads: Int, val allowSpuriousWakeUps: Boolean = false) : MonitorTracker {
    // Maintains a set of acquired monitors with an information on which thread
    // performed the acquisition and the reentrancy depth.
    private val acquiredMonitors = mutableMapOf<OpaqueValue, MonitorAcquiringInfo>()
    // Maintains a set of monitors on which each thread is waiting.
    // Note, that a thread can wait on a free monitor if it is waiting for
    // a `notify` call.
    // Stores `null` if thread is not waiting on any monitor.
    private val acquiringMonitors = Array<MonitorAcquiringInfo?>(nThreads) { null }
    // Stores `true` for the threads which are waiting for a
    // `notify` call on the monitor stored in `acquiringMonitor`.
    private val waitForNotify = BooleanArray(nThreads) { false }

    /**
     * Performs a logical acquisition.
     */
    override fun acquire(iThread: Int, monitor: OpaqueValue): Boolean {
        // Increment the reentrant depth and store the acquisition info if needed.
        val info = acquiredMonitors.computeIfAbsent(monitor) {
            MonitorAcquiringInfo(monitor, iThread, 0)
        }
        if (info.iThread != iThread) {
            acquiringMonitors[iThread] = MonitorAcquiringInfo(monitor, iThread, 0)
            return false
        }
        info.timesAcquired++
        acquiringMonitors[iThread] = null // re-set
        return true
    }

    /**
     * Performs a logical release.
     */
    override fun release(iThread: Int, monitor: OpaqueValue) {
        // Decrement the reentrancy depth and remove the acquisition info
        // if the monitor becomes free to acquire by another thread.
        val info = acquiredMonitors[monitor]!!
        info.timesAcquired--
        if (info.timesAcquired == 0)
            acquiredMonitors.remove(monitor)
    }

    override fun canAcquire(iThread: Int, monitor: OpaqueValue): Boolean {
        val info = acquiredMonitors[monitor] ?: return true
        return info.iThread == iThread
    }

    override fun owner(monitor: OpaqueValue): Int? =
        acquiredMonitors[monitor]?.iThread

    override fun reentranceDepth(iThread: Int, monitor: OpaqueValue): Int {
        val info = acquiredMonitors[monitor]?.takeIf { it.iThread == iThread } ?: return 0
        return info.timesAcquired
    }

    /**
     * Performs a logical wait, [isWaiting] for the specified thread
     * returns `true` until the corresponding [notify] or [notifyAll] is invoked.
     */
    override fun wait(iThread: Int, monitor: OpaqueValue): Boolean {
        var info = acquiredMonitors[monitor]
        if (info != null && info.iThread == iThread) {
            /* in case when current thread owns the lock we do not
             * proceed immediately even if spurious wake-ups are allowed,
             * we release the lock in order to give other thread a chance to acquire it
             */
            waitForNotify[iThread] = true
            acquiringMonitors[iThread] = info
            acquiredMonitors.remove(monitor)
            return true
        }
        info = acquiringMonitors[iThread]
        check(info != null && info.monitor == monitor && info.iThread == iThread) {
            "Monitor should have been acquired by this thread"
        }
        if (waitForNotify[iThread] && !allowSpuriousWakeUps) {
            return true
        }
        acquiredMonitors[monitor] = info
        acquiringMonitors[iThread] = null
        return false
    }

    /**
     * Performs the logical `notify` operation.
     * Notifies all threads, even if [notifyAll] is false: odd threads will have a spurious wakeup.
     */
    override fun notify(iThread: Int, monitor: OpaqueValue, notifyAll: Boolean) {
        acquiringMonitors.forEachIndexed { t, info ->
            if (monitor == info?.monitor)
                waitForNotify[t] = false
        }
    }

    /**
     * Returns `true` if the corresponding threads is waiting on some monitor.
     */
    override fun isWaiting(iThread: Int): Boolean {
        val monitor = acquiringMonitors[iThread]?.monitor ?: return false
        if (waitForNotify[iThread] && !allowSpuriousWakeUps)
            return true
        return !canAcquire(iThread, monitor)
    }

    override fun reset() {
        acquiredMonitors.clear()
        acquiringMonitors.fill(null)
        waitForNotify.fill(false)
    }

    fun copy(): MapMonitorTracker {
        val tracker = MapMonitorTracker(nThreads, allowSpuriousWakeUps)
        acquiredMonitors.forEach { (monitor, info) ->
            tracker.acquiredMonitors[monitor] = info.copy()
        }
        acquiringMonitors.forEachIndexed { thread, info ->
            tracker.acquiringMonitors[thread] = info?.copy()
        }
        waitForNotify.copyInto(tracker.waitForNotify)
        return tracker
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        return (other is MapMonitorTracker) &&
            (nThreads == other.nThreads) &&
            (allowSpuriousWakeUps == other.allowSpuriousWakeUps) &&
            (acquiredMonitors == other.acquiredMonitors) &&
            (acquiringMonitors.contentEquals(other.acquiringMonitors)) &&
            (waitForNotify.contentEquals(other.waitForNotify))
    }

    override fun hashCode(): Int {
        var result = acquiredMonitors.hashCode()
        result = 31 * result + acquiringMonitors.contentHashCode()
        result = 31 * result + waitForNotify.contentHashCode()
        return result
    }

}

/**
 * Stores the number of reentrant acquisitions ([timesAcquired])
 * and the number of thread ([iThread]) that holds the monitor.
 */
private data class MonitorAcquiringInfo(val monitor: OpaqueValue, val iThread: Int, var timesAcquired: Int)