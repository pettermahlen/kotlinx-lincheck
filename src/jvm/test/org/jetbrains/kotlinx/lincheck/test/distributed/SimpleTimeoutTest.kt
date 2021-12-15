/*
 * Lincheck
 *
 * Copyright (C) 2019 - 2021 JetBrains s.r.o.
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

package org.jetbrains.kotlinx.lincheck.test.distributed.serverclientstorage

import kotlinx.coroutines.channels.Channel
import kotlinx.coroutines.channels.Channel.Factory.UNLIMITED
import kotlinx.coroutines.sync.Semaphore
import org.jetbrains.kotlinx.lincheck.LinChecker
import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.distributed.DistributedOptions
import org.jetbrains.kotlinx.lincheck.distributed.Environment
import org.jetbrains.kotlinx.lincheck.distributed.Node
import org.jetbrains.kotlinx.lincheck.distributed.createDistributedOptions
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test

sealed class PingPongMessage
object Ping : PingPongMessage() {
    override fun toString() = "Ping"
}

object Pong : PingPongMessage() {
    override fun toString() = "Pong"
}

class UnreliablePingPongServer(val env: Environment<PingPongMessage, Unit>) : Node<PingPongMessage, Unit> {
    var shouldSkip = true
    override fun onMessage(message: PingPongMessage, sender: Int) {
        check(message is Ping) {
            "Unexpected message type"
        }
        if (shouldSkip) {
            shouldSkip = false
            return
        }
        env.send(Pong, sender)
        shouldSkip = true
    }
}

class PingPongClient(val env: Environment<PingPongMessage, Unit>) : Node<PingPongMessage, Unit> {
    private val channel = Channel<PingPongMessage>(UNLIMITED)
    private val server = env.getAddressesForClass(UnreliablePingPongServer::class.java)!![0]
    override fun onMessage(message: PingPongMessage, sender: Int) {
        check(message is Pong) {
            "Unexpected message type"
        }
        channel.offer(message)
    }

    @Operation(cancellableOnSuspension = false)
    suspend fun ping(): Boolean {
        while (true) {
            var hasResult = false
            env.send(Ping, server)
            val res = env.withTimeout(50) {
                channel.receive()
                hasResult = true
            }
            check(hasResult == (res != null))
            //println("[${env.nodeId}]: Exit timeout $res")
            if (hasResult) return true
        }
    }
}

class PingPongMock : VerifierState() {
    override fun extractState() = true
    suspend fun ping() = true
}

class SimpleTimeoutTest {
    @Test
    fun test() = createDistributedOptions<PingPongMessage>()
        .nodeType(PingPongClient::class.java, 1)
        .nodeType(UnreliablePingPongServer::class.java, 1)
        .requireStateEquivalenceImplCheck(false)
        .invocationsPerIteration(30_000)
        .iterations(500)
        .sequentialSpecification(PingPongMock::class.java)
        .actorsPerThread(2)
        .storeLogsForFailedScenario("ping_pong_timeout.txt")
        .check()
}