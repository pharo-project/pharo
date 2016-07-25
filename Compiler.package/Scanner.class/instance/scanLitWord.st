expression types
scanLitWord
	"Accumulate keywords and asSymbol the result."

	| t |
	[(self typeTableAt: hereChar) = #xLetter]
		whileTrue: 
			[t := token.
			self xLetter.
			token := t , token].
	token := token asSymbol