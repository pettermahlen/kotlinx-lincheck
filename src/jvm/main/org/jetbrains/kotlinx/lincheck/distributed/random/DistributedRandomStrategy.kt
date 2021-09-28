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

package org.jetbrains.kotlinx.lincheck.distributed.random

import org.jetbrains.kotlinx.lincheck.distributed.*
import org.jetbrains.kotlinx.lincheck.execution.ExecutionResult
import org.jetbrains.kotlinx.lincheck.execution.ExecutionScenario
import org.jetbrains.kotlinx.lincheck.runner.CompletedInvocationResult
import org.jetbrains.kotlinx.lincheck.strategy.IncorrectResultsFailure
import org.jetbrains.kotlinx.lincheck.strategy.LincheckFailure
import org.jetbrains.kotlinx.lincheck.strategy.toLincheckFailure
import org.jetbrains.kotlinx.lincheck.verifier.Verifier
import java.lang.reflect.Method

fun ExecutionResult.newResult(stateRepresentation: String?): ExecutionResult = ExecutionResult(
    initResults,
    afterInitStateRepresentation,
    parallelResultsWithClock,
    stateRepresentation,
    postResults,
    afterPostStateRepresentation
)

internal class DistributedRandomStrategy<Message, Log>(
    testCfg: DistributedCTestConfiguration<Message, Log>,
    testClass: Class<*>,
    scenario: ExecutionScenario,
    validationFunctions: List<Method>,
    stateRepresentationFunction: Method?,
    verifier: Verifier
) : DistributedStrategy<Message, Log>(
    testCfg,
    testClass,
    scenario,
    validationFunctions,
    stateRepresentationFunction,
    verifier
) {
    private val probability = Probability(testCfg, generatingRandom)
    private val runner = DistributedRunner(this, testCfg, testClass, validationFunctions, stateRepresentationFunction)

    init {
        try {
            runner.initialize()
        } catch (t: Throwable) {
            runner.close()
            throw t
        }
    }


    private fun tryCrash(iNode: Int) {
        if (probability.nodeFailed(crashInfo.remainedNodeCount())) {
            crashInfo.crashNode(iNode)
            throw CrashError()
        }
    }

    override fun onMessageSent(iNode: Int, event: MessageSentEvent<Message>) {
        tryCrash(iNode)
    }

    override fun beforeLogModify(iNode: Int) {
        tryCrash(iNode)
    }

    override fun next(taskManager: TaskManager): Task? {
        val tasks = taskManager.getAvailableTasks()
        if (tasks.isEmpty() || runner.hasAllResults() && tasks.all { it.value is Timer }) return null
        val id = tasks.keys.random(generatingRandom)
        return taskManager.getTaskById(id)
    }

    override fun run(): LincheckFailure? {
        println(scenario)
        runner.use { runner ->
            // Run invocations
            for (invocation in 0 until testCfg.invocationsPerIteration) {
                if (invocation % 1000 == 0) println("INVOCATION $invocation")
                val crashExpectation = if (testCfg.supportRecovery == CrashMode.NO_CRASHES) 0 else {
                    3
                }
                probability.reset(crashExpectation)
                val ir = runner.run()
                when (ir) {
                    is CompletedInvocationResult -> {
                        if (!verifier.verifyResults(scenario, ir.results)) {
                            val stateRepresentation = runner.constructStateRepresentation()
                            return IncorrectResultsFailure(
                                scenario,
                                ir.results.newResult(stateRepresentation)
                            ).also {
                                runner.storeEventsToFile(it)
                            }
                        }
                    }
                    else -> {
                        return ir.toLincheckFailure(scenario).also {
                            runner.storeEventsToFile(it)
                        }
                    }
                }
            }
            return null
        }
    }

    override fun tryAddCrashBeforeSend(iNode: Int, event: MessageSentEvent<Message>) {
        tryCrash(iNode)
    }

    override fun tryAddPartitionBeforeSend(iNode: Int, event: MessageSentEvent<Message>): Boolean {
        //TODO
        return false
    }

    override fun getMessageRate(iNode: Int, event: MessageSentEvent<Message>): Int = probability.duplicationRate()
}