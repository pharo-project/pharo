scanning
scanMultibyteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 
"this is a scanning method for
multibyte characters in a WideString
a font that does not do character-pair kerning"
	| char ascii encoding nextDestX startEncoding |
	lastIndex := startIndex.
	startEncoding := (sourceString at: startIndex) leadingChar.
	[lastIndex <= stopIndex] whileTrue: [
		char := sourceString at: lastIndex.
		encoding := char leadingChar.
		encoding ~= startEncoding
			ifTrue: [lastIndex := lastIndex - 1. ^#endOfRun].
		ascii := char charCode.
		(ascii < 256 and: [(stopConditions at: ascii + 1) ~~ nil])
			ifTrue: [^ stopConditions at: ascii + 1].
		"bump nextDestX by the width of the current character"
		nextDestX := destX + (font widthOf: char).
		nextDestX > rightX ifTrue: [^#crossedX].
		destX := nextDestX + kern .
		lastIndex := lastIndex + 1.
	].
	^self handleEndOfRunAt: stopIndex