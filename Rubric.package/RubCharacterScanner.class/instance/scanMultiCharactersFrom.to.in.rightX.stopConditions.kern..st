scanning
scanMultiCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernDelta

	| ascii encoding f nextDestX maxAscii startEncoding floatDestX widthAndKernedWidth nextChar atEndOfRun |
	lastIndex := startIndex.
	lastIndex > stopIndex ifTrue: [lastIndex := stopIndex. ^ stops endOfRun].
	startEncoding := (sourceString at: startIndex) leadingChar.
	font ifNil: [font := TextStyle defaultFont fontArray at: 1].
	font isFontSet ifTrue: [
		f := [font fontArray at: startEncoding + 1]
			on: Exception do: [:ex | nil].
		f ifNil: [ f := font fontArray at: 1].
		maxAscii := f maxAscii.
		spaceWidth := f widthOf: Character space.
	] ifFalse: [
		maxAscii := font maxAscii.
	].
	floatDestX := destX.
	widthAndKernedWidth := Array new: 2.
	atEndOfRun := false.
	[lastIndex <= stopIndex] whileTrue: [
		encoding := (sourceString at: lastIndex) leadingChar.
		encoding ~= startEncoding ifTrue: [lastIndex := lastIndex - 1. ^ stops endOfRun].
		ascii := (sourceString at: lastIndex) charCode.
		ascii > maxAscii ifTrue: [ascii := maxAscii].
		(encoding = 0 and: [ascii < stops size and: [(stops at: ascii + 1) ~~ nil]]) ifTrue: [^ stops at: ascii + 1].
		(self isBreakableAt: lastIndex in: sourceString in: Latin1Environment) ifTrue: [
			self registerBreakableIndex.
		].
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
			widthAndKernedWidthOfLeft: (sourceString at: lastIndex) 
			right: nextChar
			into: widthAndKernedWidth.
		nextDestX := floatDestX + (widthAndKernedWidth at: 1).
		nextDestX > rightX ifTrue: [destX ~= firstDestX ifTrue: [^stops crossedX]].
		floatDestX := floatDestX + kernDelta + (widthAndKernedWidth at: 2).
		atEndOfRun 
			ifTrue:[
				pendingKernX := (widthAndKernedWidth at: 2) - (widthAndKernedWidth at: 1).
				floatDestX := floatDestX - pendingKernX].
		destX := floatDestX .
		lastIndex := lastIndex + 1.
	].
	lastIndex := stopIndex.
	^ stops endOfRun