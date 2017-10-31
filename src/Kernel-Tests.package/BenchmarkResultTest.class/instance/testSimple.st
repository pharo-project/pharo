tests
testSimple
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
	
