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
package org.jetbrains.kotlinx.lincheck.strategy.managed

import org.jetbrains.kotlinx.lincheck.*
import org.jetbrains.kotlinx.lincheck.execution.*
import org.jetbrains.kotlinx.lincheck.verifier.*

/**
 * Some atomic primitives are common and can be analyzed from a higher level
 * of abstraction or can not be transformed (i.e, Unsafe or AFU).
 * Thus, we do not transform them and improve the trace representation.
 *
 * For example, in the execution trace where `AtomicLong.get()` happens,
 * we print the code location where this atomic method is called
 * instead of going deeper inside it.
 */
private fun isTrustedPrimitive(className: String) =
    className == "java.lang.invoke.VarHandle" ||
    className == "sun.misc.Unsafe" ||
    className == "jdk.internal.misc.Unsafe" ||
    className.startsWith("java.util.concurrent.atomic.Atomic") || // AFUs and Atomic[Integer/Long/...]
    className.startsWith("kotlinx.atomicfu.Atomic")
