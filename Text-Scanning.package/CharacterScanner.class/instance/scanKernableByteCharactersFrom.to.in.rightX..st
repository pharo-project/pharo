scanning
scanKernableByteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 
"this is a scanning method for
single byte characters in a ByteString
a font that does do character-pair kerning via widthAndKernedWidthOfLeft:right:into:"
	| ascii nextDestX char floatDestX widthAndKernedWidth nextCharOrNil atEndOfRun |
	lastIndex := startIndex.
	floatDestX := destX.
	widthAndKernedWidth := Array new: 2.
	atEndOfRun := false.
	[lastIndex <= stopIndex]
		whileTrue: [
			"get the character value"
			char := sourceString at: lastIndex.
			ascii := char asciiValue + 1.
			"if there is an entry in 'stops' for this value, return it"
			(stopConditions at: ascii)
				ifNotNil: [^ stopConditions at: ascii].
			"get the next character..."
			nextCharOrNil := lastIndex + 1 <= stopIndex
						ifTrue: [sourceString at: lastIndex + 1]
						ifFalse: ["if we're at or past the stopIndex, see if there is anything in the full string"
							atEndOfRun := true.
							lastIndex + 1 <= sourceString size
								ifTrue: [sourceString at: lastIndex + 1]].
			"get the font's kerning info for the pair of current character and next character"
			"for almost all fonts in common use this is a waste of time since they don't support pair kerning and both values are #widthOf: char"
			font
				widthAndKernedWidthOfLeft: char
				right: nextCharOrNil
				into: widthAndKernedWidth.
			"bump nextDestX by the width of the current character"
			nextDestX := floatDestX
						+ (widthAndKernedWidth at: 1).
			"if the next x is past the right edge, return crossedX"
			nextDestX > rightX
				ifTrue: [^ #crossedX].
			"bump floatDestX by the *kerned* width of the current
			character, which is where the *next* char will go"
			floatDestX := floatDestX + kern
						+ (widthAndKernedWidth at: 2).
			"if we are at the end of this run we keep track of the
			character-kern-delta for possible later use and then rather
			insanely remove that character-kern-delta from floatDestX,
			making it equivalent to (old floatDestX) + kernDelta +
			width-of-character - no idea why"
			atEndOfRun
				ifTrue: [pendingKernX := (widthAndKernedWidth at: 2)
								- (widthAndKernedWidth at: 1).
					floatDestX := floatDestX - pendingKernX].
			"save the next x for next time around the loop"
			destX := floatDestX.
			lastIndex := lastIndex + 1].
	^self handleEndOfRunAt: stopIndex
