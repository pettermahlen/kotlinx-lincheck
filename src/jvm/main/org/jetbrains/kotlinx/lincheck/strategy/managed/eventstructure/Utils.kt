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

package org.jetbrains.kotlinx.lincheck.strategy.managed.eventstructure

fun <K, V> MutableMap<K, V>.update(key: K, default: V, transform: (V) -> V) {
    // TODO: could it be done with a single lookup in a map?
    put(key, get(key)?.let(transform) ?: default)
}

fun <K, V> Map<K, V>.mergeReduce(other: Map<K, V>, reduce: (V, V) -> V): MutableMap<K, V> =
    toMutableMap().apply { other.forEach { (key, value) ->
        update(key, default = value) { reduce(it, value) }
    }}

fun <T> List<T>.getSquashed(position: Int, combine: (T, T) -> T?): Pair<T, Int>? {
    var i = position
    var accumulator = getOrNull(position) ?: return null
    while (++i < size) {
        accumulator = combine(accumulator, get(i)) ?: break
    }
    return accumulator to i
}

fun <T> List<T>.squash(combine: (T, T) -> T?): List<T> {
    if (isEmpty()) return emptyList()
    val squashed = arrayListOf<T>()
    var position = 0
    while (position < size) {
        val (element, nextPosition) = getSquashed(position, combine)!!
        squashed.add(element)
        position = nextPosition
    }
    return squashed
}

infix fun Boolean.implies(other: Boolean): Boolean = !this || other

class UnreachableException: Exception()

fun unreachable(): Nothing {
    throw UnreachableException()
}