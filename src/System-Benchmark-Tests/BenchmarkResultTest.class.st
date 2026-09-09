"
Unit test for BenchmarkResult
"
Class {
	#name : 'BenchmarkResultTest',
	#superclass : 'TestCase',
	#category : 'System-Benchmark-Tests',
	#package : 'System-Benchmark-Tests'
}

{ #category : 'tests' }
BenchmarkResultTest >> testBenchFib [
	self assert: 0 benchFib equals: 1.
	self assert: 1 benchFib equals: 1.
	self assert: 2 benchFib equals: 3
]

{ #category : 'tests' }
BenchmarkResultTest >> testBenchFor [
	| benchmarkResult duration minimumIterations |
	duration := 500 milliSeconds.
	benchmarkResult := [ 100 factorial ] benchFor: duration.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult elapsedTime >= duration.
	"These should pass even on a very slow machine or simulation"
	minimumIterations := 10.
	self assert: benchmarkResult iterations > minimumIterations.
	self assert: benchmarkResult period < (duration / minimumIterations)
]

{ #category : 'tests' }
BenchmarkResultTest >> testBenchForException [

	"benchFor: expects a duration. Check that the bencher does not go in an infinite loop if the wakeup thread fails."

	self should: [ [ 100 factorial ] benchFor: 1 ] raise: Error
]

{ #category : 'tests' }
BenchmarkResultTest >> testEmpty [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	self assert: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations isNil.
	self assert: benchmarkResult elapsedTime isNil.
	"cover the code at least"
	self deny: benchmarkResult shortPrintString isEmpty.
	self deny: benchmarkResult printString isEmpty
]

{ #category : 'tests' }
BenchmarkResultTest >> testOne [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 1.
	benchmarkResult elapsedTime: 1 second.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 1.
	self assert: benchmarkResult elapsedTime equals: 1 second.
	self assert: benchmarkResult frequency equals: 1.
	self assert: benchmarkResult executionsPerSecond equals: 1.
	self assert: benchmarkResult period equals: 1 second.
	self assert: benchmarkResult timePerExecution equals: 1 second.
	"cover the code at least"
	self deny: benchmarkResult shortPrintString isEmpty.
	self deny: benchmarkResult printString isEmpty
]

{ #category : 'tests' }
BenchmarkResultTest >> testOneOverTwo [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 1.
	benchmarkResult elapsedTime: 2 second.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 1.
	self assert: benchmarkResult elapsedTime equals: 2 second.
	self assert: benchmarkResult frequency equals: 0.5.
	self assert: benchmarkResult executionsPerSecond equals: 0.5.
	self assert: benchmarkResult period equals: 2 second.
	self assert: benchmarkResult timePerExecution equals: 2 second.
	"cover the code at least"
	self assert: benchmarkResult shortPrintString equals: '0.500 per second'.
	self deny: benchmarkResult printString isEmpty
]

{ #category : 'tests' }
BenchmarkResultTest >> testSimple [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 100.
	benchmarkResult elapsedTime: 4 seconds.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 100.
	self assert: benchmarkResult elapsedTime equals: 4 seconds.
	self assert: benchmarkResult frequency equals: 25.
	self assert: benchmarkResult executionsPerSecond equals: 25.
	self assert: benchmarkResult period equals: 40 milliSeconds.
	self assert: benchmarkResult timePerExecution equals: 40 milliSeconds.
	"cover the code at least"
	self assert: benchmarkResult shortPrintString equals: '25.000 per second'.
	self deny: benchmarkResult printString isEmpty
]
