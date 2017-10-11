running
randomCodePointBetween: lower and: upper
	| max span codePoint |
	max := upper min: 16rE01EF.
	span := max - lower + 1.
	[self nonCharacters includes: (codePoint := (self generator next * span) floor + lower) ] whileTrue.
	^ codePoint