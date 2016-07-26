evaluating
bench
	"Return how many times the receiver can get executed in 5 seconds. 
	Answer a string with meaningful description.
	See #benchFor: which returns a BenchmarkResult"
	
	"[3.14 printString] bench"
	
	| benchmarkResult |
	benchmarkResult := self benchFor: 5 seconds.
	^ benchmarkResult shortPrintString 