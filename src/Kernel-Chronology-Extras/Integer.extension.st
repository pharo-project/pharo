Extension { #name : #Integer }

{ #category : #'*Kernel-Chronology-Extras' }
Integer >> asYear [

 	^ Year year: self  
]

{ #category : #'*Kernel-Chronology-Extras' }
Integer >> tinyBenchmarks [
	
	| t1 t2 r n1 n2 |
	n1 := 1.
	[t1 := Time millisecondsToRun: [n1 benchmark].
	t1 < 1000] whileTrue:[n1 := n1 * 2]. "Note: #benchmark's runtime is about O(n)"

	n2 := 28.
	[t2 := Time millisecondsToRun: [r := n2 benchFib].
	t2 < 1000] whileTrue:[n2 := n2 + 1]. "Note: #benchFib's runtime is about O(n^2)."

	^ ((n1 * 500000 * 1000) // t1) printString, ' bytecodes/sec; ',
	  ((r * 1000) // t2) printString, ' sends/sec'
]
