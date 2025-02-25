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
import org.jetbrains.kotlinx.lincheck.checkImpl
import org.jetbrains.kotlinx.lincheck.strategy.managed.modelchecking.ModelCheckingOptions
import org.jetbrains.kotlinx.lincheck.test.*
import org.jetbrains.kotlinx.lincheck.verifier.VerifierState
import org.junit.Test

/**
 * This test checks that values captured in an incorrect interleaving have proper representation.
 * `toString` method is used only for primitive types and their wrappers.
 * For other classes we use simplified representation to avoid problems with concurrent modification or
 * not completely initialized objects (e.g, with `ConcurrentModificationException`)
 */
class CapturedValueRepresentationTest : VerifierState() {
    private var counter = 0
    private var outerClass1 = OuterDataClass(0)
    private var outerClass2 = OuterDataClass(0)
    private var innerClass = InnerClass()
    private var otherInnerClass = InnerClass()
    private var primitiveArray = IntArray(1)
    private var objectArray = Array(1) { "" }

    @Operation
    fun operation(): Int {
        outerClass1
        outerClass2
        innerClass
        innerClass
        otherInnerClass
        primitiveArray
        objectArray
        return counter++
    }

    @Test
    fun test() {
        val failure = ModelCheckingOptions()
            .actorsAfter(0)
            .actorsBefore(0)
            .actorsPerThread(1)
            .checkImpl(this::class.java)
        checkNotNull(failure) { "test should fail" }
        val log = failure.toString()
        check(" OuterDataClass@1" in log)
        check(" InnerClass@1" in log)
        check(log.split(" InnerClass@1").size - 1 == 2) { "two reads of innerClass should return same result" }
        check(" InnerClass@2" in log) { "Two different InnerClass objects were read, but the same was reported" }
        check(" int[]@1" in log)
        check(" String[]@1" in log)
        check(" 0" in log)
        check(" OuterDataClass@2" in log) { "Equal but not same object should have different numbers" }
        checkTraceHasNoLincheckEvents(log)
    }

    override fun extractState(): Any = counter

    private class InnerClass
}

private data class OuterDataClass(val a: Int)
