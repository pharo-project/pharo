running
randomCodePointBetween: lower and: upper
	| max span codePoint |
	max := upper min: 16rE01EF.
	span := max - lower + 1.
	[
		codePoint := (self generator next * span) floor + lower.
		Unicode isNonCharacter: (Character codePoint: codePoint) 
	] whileTrue.
	^ codePoint