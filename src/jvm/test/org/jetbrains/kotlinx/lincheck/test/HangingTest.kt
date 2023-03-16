/*-
 * #%L
 * Lincheck
 * %%
 * Copyright (C) 2019 JetBrains s.r.o.
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
package org.jetbrains.kotlinx.lincheck.test

import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.annotations.*
import org.jetbrains.kotlinx.lincheck.strategy.*

class HangingInParallelPartTest : AbstractLincheckTest(DeadlockWithDumpFailure::class) {

    @Operation
    fun hang() {
        while (true) {}
    }

    override fun <O : Options<O, *>> O.customize() {
        iterations(1)
        actorsBefore(0)
        actorsAfter(0)
        threads(2)
        actorsPerThread(2)
        requireStateEquivalenceImplCheck(false)
        minimizeFailedScenario(false)
        invocationTimeout(100)
    }

}

class HangingInInitPartTest : AbstractLincheckTest(DeadlockWithDumpFailure::class) {

    @Operation
    fun hang() {
        while (true) {}
    }

    @Operation
    fun idle() {}

    val scenario = scenario {
        initial {
            actor(HangingInInitPartTest::hang)
        }
        parallel {
            thread {
                actor(HangingInInitPartTest::idle)
            }
        }
    }

    override fun <O : Options<O, *>> O.customize() {
        addCustomScenario(scenario)
        iterations(0)
        requireStateEquivalenceImplCheck(false)
        minimizeFailedScenario(false)
        invocationTimeout(100)
    }

}

class HangingInPostPartTest : AbstractLincheckTest(DeadlockWithDumpFailure::class) {

    @Operation
    fun hang() {
        while (true) {}
    }

    @Operation
    fun idle() {}

    val scenario = scenario {
        parallel {
            thread {
                actor(HangingInPostPartTest::idle)
            }
        }
        post {
            actor(HangingInPostPartTest::hang)
        }
    }

    override fun <O : Options<O, *>> O.customize() {
        addCustomScenario(scenario)
        iterations(0)
        requireStateEquivalenceImplCheck(false)
        minimizeFailedScenario(false)
        invocationTimeout(100)
    }

}