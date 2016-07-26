file in/out
displayChar: ascii form: charForm 
	"Convenience utility used during conversion of BitFont files"
	| m bigForm |
	Display fillBlack: (0 @ 0 extent: 20 @ 14).
	ascii printString displayAt: 0 @ 2.
	charForm width > 0 ifTrue: 
		[ m := 5.
		bigForm := charForm 
			magnify: charForm boundingBox
			by: m @ m.
		Display 
			border: ((bigForm boundingBox expandBy: m) translateBy: 50 @ 2)
			width: m.
		bigForm displayAt: 50 @ 2.
		Display fillBlack: (50 @ 2 + ((m * charForm width) @ 0) extent: 1 @ (m * self height)) ]