enumerating
timesRepeat: aBlock 
	"Evaluate the argument, aBlock, the number of times represented by the 
	receiver."

	| count |
	count := 1.
	[count <= self]
		whileTrue: 
			[aBlock value.
			count := count + 1]