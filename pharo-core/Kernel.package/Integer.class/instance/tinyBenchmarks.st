benchmarks
tinyBenchmarks
	"Report the results of running the two tiny Squeak benchmarks.
	ar 9/10/1999: Adjusted to run at least 1 sec to get more stable results"
	"0 tinyBenchmarks"
	"On a 292 MHz G3 Mac: 22727272 bytecodes/sec; 984169 sends/sec"
	"On a 400 MHz PII/Win98:  18028169 bytecodes/sec; 1081272 sends/sec"
	| t1 t2 r n1 n2 |
	n1 := 1.
	[t1 := Time millisecondsToRun: [n1 benchmark].
	t1 < 1000] whileTrue:[n1 := n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 := 28.
	[t2 := Time millisecondsToRun: [r := n2 benchFib].
	t2 < 1000] whileTrue:[n2 := n2 + 1]. "Note: #benchFib's runtime is about O(n^2)."

	^ ((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printString, ' sends/sec'