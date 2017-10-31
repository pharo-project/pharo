tests
testEmpty
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	self assert: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations isNil.
	self assert: benchmarkResult elapsedTime isNil.
	"cover the code at least"
	self deny: benchmarkResult shortPrintString isEmpty.
	self deny: benchmarkResult printString isEmpty
