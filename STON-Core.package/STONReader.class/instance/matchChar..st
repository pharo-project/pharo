private
matchChar: character
	"Tries to match character, consume input and 
	answer true if successful and consumes whitespace at the end."

	^ (readStream peekFor: character)
		ifTrue: [ 
			self consumeWhitespace.
			true ]
		ifFalse: [ false ]