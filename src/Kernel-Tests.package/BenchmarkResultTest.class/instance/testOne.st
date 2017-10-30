tests
testOne
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
	
