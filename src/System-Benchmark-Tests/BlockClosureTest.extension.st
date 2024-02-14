Extension { #name : 'BlockClosureTest' }

{ #category : '*System-Benchmark-Tests' }
BlockClosureTest >> testBenchFor [
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

{ #category : '*System-Benchmark-Tests' }
BlockClosureTest >> testBenchForException [

	"benchFor: expects a duration. Check that the bencher does not go in an infinite loop if the wakeup thread fails."

	self should: [ [ 100 factorial ] benchFor: 1 ] raise: Error
]
