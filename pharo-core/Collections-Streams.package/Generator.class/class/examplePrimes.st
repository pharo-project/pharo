examples
examplePrimes
	| generator |
	Transcript open.
	generator := Generator on: [:g| Integer primesUpTo: 100 do:[:prime| g yield: prime]].
	[generator atEnd] whileFalse:[Transcript show: generator next; cr].