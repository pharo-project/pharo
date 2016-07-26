scanner methods
scanJapaneseCharactersFrom: startIndex to: stopIndex in: sourceString rightX: rightX stopConditions: stops kern: kernDelta

	| ascii encoding f nextDestX maxAscii startEncoding |
	lastIndex := startIndex.
	lastIndex > stopIndex ifTrue: [lastIndex := stopIndex. ^ stops endOfRun].
	startEncoding := (sourceString at: startIndex) leadingChar.
	font ifNil: [font := TextStyle defaultFont fontArray at: 1].
	font isFontSet ifTrue: [
		f := [font fontArray at: startEncoding + 1]
			on: Exception do: [:ex | nil].
		f ifNil: [ f := font fontArray at: 1].
		maxAscii := f maxAscii.
		"xTable := f xTable.
		maxAscii := xTable size - 2."
		spaceWidth := f widthOf: Character space.
	] ifFalse: [
		maxAscii := font maxAscii.
	].
	[lastIndex <= stopIndex] whileTrue: [
		"self halt."
		encoding := (sourceString at: lastIndex) leadingChar.
		encoding ~= startEncoding ifTrue: [lastIndex := lastIndex - 1. ^ stops endOfRun].
		ascii := (sourceString at: lastIndex) charCode.
		ascii > maxAscii ifTrue: [ascii := maxAscii].
		(encoding = 0 and: [ascii < stops size and: [(stops at: ascii + 1) ~~ nil]]) ifTrue: [^ stops at: ascii + 1].
		(self isBreakableAt: lastIndex in: sourceString in: (EncodedCharSet charsetAt: encoding)) ifTrue: [
			self registerBreakableIndex.
		].
		nextDestX := destX + (font widthOf: (sourceString at: lastIndex)).
		nextDestX > rightX ifTrue: [firstDestX ~= destX ifTrue: [^ stops crossedX]].
		destX := nextDestX + kernDelta.
		lastIndex := lastIndex + 1.
	].
	lastIndex := stopIndex.
	^ stops endOfRun