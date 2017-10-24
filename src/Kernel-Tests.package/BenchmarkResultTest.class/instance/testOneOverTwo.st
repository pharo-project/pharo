tests
testOneOverTwo
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
	
