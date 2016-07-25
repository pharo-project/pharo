bytecode generation
genPushNClosureTemps: numTemps
	"	230		11100110	iiiiiiii		PushNClosureTemps iiiiiiii"
	(numTemps < 0 or: [numTemps >= 64]) ifTrue:
		[^self outOfRangeError: 'closure num temps' index: numTemps range: 0 to: 127].
	stream nextPut: 230.
	stream nextPut: numTemps