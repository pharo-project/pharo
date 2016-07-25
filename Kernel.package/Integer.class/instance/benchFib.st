benchmarks
benchFib  "Handy send-heavy benchmark"
	"(result // seconds to run) = approx calls per second"
	" | r t |
	  t := Time millisecondsToRun: [r := 26 benchFib].
	  (r * 1000) // t"
	"138000 on a Mac 8100/100"
	^ self < 2
		ifTrue: [1] 
		ifFalse: [(self-1) benchFib + (self-2) benchFib + 1]
