lint
whileTrue
	| i |
	i := 1.
	[i < self size] whileTrue: 
			[(self at: i) printString.
			i := i + 1]