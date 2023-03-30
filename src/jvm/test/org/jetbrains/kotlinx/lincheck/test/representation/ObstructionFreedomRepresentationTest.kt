/*-
 * #%L
 * Lincheck
 * %%
 * Copyright (C) 2019 - 2020 JetBrains s.r.o.
 * %%
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
 * <http://www.gnu.org/licenses/lgpl-3.0.html>.
 * #L%
 */

package org.jetbrains.kotlinx.lincheck.test.representation

import org.jetbrains.kotlinx.lincheck.annotations.Operation
import org.jetbrains.kotlinx.lincheck.appendFailure
import org.jetbrains.kotlinx.lincheck.checkImpl
import org.jetbrains.kotlinx.lincheck.strategy.managed.ManagedCTestConfiguration
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.ModelCheckingOptions
import org.jetbrains.kotlinx.lincheck.test.checkTraceHasNoLincheckEvents
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Ignore
import org.junit.Test
import java.util.concurrent.atomic.AtomicInteger

/**
 * This test checks that the last event in the case of an active lock
 * that causes obstruction freedom violation is reported.
 */
@Ignore
class ObstructionFreedomActiveLockRepresentationTest : VerifierState() {
    private val counter = AtomicInteger(0)

    @Operation
    fun operation() {
        // The first invocation will not fail
        incrementManyTimes()
        // This invocation will fail immediately after `counter.get`,
        // but obstruction freedom violation will be caused by `counter.incrementAndGet`
        incrementManyTimes()
    }

    fun incrementManyTimes() {
        counter.get()
        // repeat exactly the maximum number of times that does not cause obstruction freedom violation
        repeat(ManagedCTestConfiguration.DEFAULT_HANGING_DETECTION_THRESHOLD) {
            counter.incrementAndGet()
        }
    }

    override fun extractState(): Any = counter.get()

    @Test
    fun test() {
        val options = ModelCheckingOptions()
            .actorsPerThread(1)
            .actorsBefore(0)
            .actorsAfter(0)
            .threads(1)
            .checkObstructionFreedom(true)
        val failure = options.checkImpl(this::class.java)
        check(failure != null) { "the test should fail" }
        val log = StringBuilder().appendFailure(failure).toString()
        check("incrementAndGet" in log) { "The cause of the error should be reported" }
        checkTraceHasNoLincheckEvents(log)
    }
}

/**
 * This test checks that the last MONITORENTER event
 * that causes obstruction freedom violation is reported.
 */
class ObstructionFreedomSynchronizedRepresentationTest : VerifierState() {
    private var counter = 0

    @Operation
    fun operation(): Int = synchronized(this) { counter++ }

    override fun extractState(): Any = counter

    @Test
    fun test() {
        val options = ModelCheckingOptions()
            .actorsPerThread(1)
            .actorsBefore(0)
            .actorsAfter(0)
            .threads(2)
            .checkObstructionFreedom(true)
        val failure = options.checkImpl(this::class.java)
        check(failure != null) { "the test should fail" }
        val log = StringBuilder().appendFailure(failure).toString()
        check(log.split("MONITORENTER").size - 1 == 2) { "MONITORENTER should be reported twice" }
        checkTraceHasNoLincheckEvents(log)
    }
}
