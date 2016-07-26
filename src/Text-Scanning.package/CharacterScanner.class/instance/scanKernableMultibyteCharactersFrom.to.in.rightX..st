scanning
scanKernableMultibyteCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX 
"this is a scanning method for
multibyte characters in a WideString
a font that does do character-pair kerning via widthAndKernedWidthOfLeft:right:into:"

	| ascii encoding nextDestX startEncoding floatDestX widthAndKernedWidth nextChar atEndOfRun char |
	lastIndex := startIndex.
	lastIndex > stopIndex ifTrue: [^self handleEndOfRunAt: stopIndex].
	startEncoding := (sourceString at: startIndex) leadingChar.
	floatDestX := destX.
	widthAndKernedWidth := Array new: 2.
	atEndOfRun := false.
	[lastIndex <= stopIndex] whileTrue: [
		char := sourceString at: lastIndex.
		encoding := char leadingChar.
		encoding ~= startEncoding
			ifTrue: [lastIndex := lastIndex - 1. ^#endOfRun].
		ascii := char charCode.
		(ascii < 256 and: [(stopConditions at: ascii + 1) ~~ nil])
			ifTrue: [^ stopConditions at: ascii + 1].
		nextChar := (lastIndex + 1 <= stopIndex) 
			ifTrue:[sourceString at: lastIndex + 1]
			ifFalse:[
				atEndOfRun := true.
				"if there is a next char in sourceString, then get the kern 
				and store it in pendingKernX"
				lastIndex + 1 <= sourceString size
					ifTrue:[sourceString at: lastIndex + 1]
					ifFalse:[	nil]].
		font 
			widthAndKernedWidthOfLeft: char 
			right: nextChar
			into: widthAndKernedWidth.
		nextDestX := floatDestX + (widthAndKernedWidth at: 1).
		nextDestX > rightX ifTrue: [^#crossedX].
		floatDestX := floatDestX + kern + (widthAndKernedWidth at: 2).
		atEndOfRun 
			ifTrue:[
				pendingKernX := (widthAndKernedWidth at: 2) - (widthAndKernedWidth at: 1).
				floatDestX := floatDestX - pendingKernX].
		destX := floatDestX .
		lastIndex := lastIndex + 1.
	].
	^self handleEndOfRunAt: stopIndex