scanning
basicScanByteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 
"this is a scanning method for
single byte characters in a ByteString
a font that does not do character-pair kerning"
	| ascii nextDestX char |
	lastIndex := startIndex.
	[lastIndex <= stopIndex]
		whileTrue: [
			"get the character value"
			char := sourceString at: lastIndex.
			ascii := char asciiValue + 1.
			"if there is an entry in 'stops' for this value, return it"
			(stopConditions at: ascii)
				ifNotNil: [^ stopConditions at: ascii].
			"bump nextDestX by the width of the current character"
			nextDestX := destX + (font widthOf: char).
			"if the next x is past the right edge, return crossedX"
			nextDestX > rightX
				ifTrue: [^#crossedX].
			"update destX and incorporate thr kernDelta"
			destX := nextDestX + kern.
			lastIndex := lastIndex + 1].
	^self handleEndOfRunAt: stopIndex

