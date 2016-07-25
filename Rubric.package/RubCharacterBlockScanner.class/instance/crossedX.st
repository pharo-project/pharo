stop conditions
crossedX
	"Text display has wrapping. The scanner just found a character past the x 
	location of the cursor. We know that the cursor is pointing at a character 
	or before one."

	| leadingTab currentX |
	characterIndex == nil ifFalse: [
		"If the last character of the last line is a space,
		and it crosses the right margin, then locating
		the character block after it is impossible without this hack."
		characterIndex > text size ifTrue: [
			lastIndex := characterIndex.
			characterPoint := (nextLeftMargin ifNil: [leftMargin]) @ (destY + line lineHeight).
			^true]].
	characterPoint x <= (destX + (lastCharacterExtent x // 2))
		ifTrue:	[lastCharacter := (text at: lastIndex).
				characterPoint := destX @ destY.
				^true].
	lastIndex >= line last 
		ifTrue:	[lastCharacter := (text at: line last).
				characterPoint := destX @ destY.
				^true].

	"Pointing past middle of a character, return the next character."
	lastIndex := lastIndex + 1.
	lastCharacter := text at: lastIndex.
	currentX := destX + lastCharacterExtent x + kern.
	self lastCharacterExtentSetX: (font widthOf: lastCharacter).
	characterPoint := currentX @ destY.
	lastCharacter = Space ifFalse: [^ true].

	"Yukky if next character is space or tab."
	alignment = Justified ifTrue:
		[self lastCharacterExtentSetX:
			(lastCharacterExtent x + 	(line justifiedPadFor: (spaceCount + 1) font: font)).
		^ true].

	true ifTrue: [^ true].
	"NOTE:  I find no value to the following code, and so have defeated it - DI"

	"See tabForDisplay for illumination on the following awfulness."
	leadingTab := true.
	line first to: lastIndex - 1 do:
		[:index | (text at: index) ~= Tab ifTrue: [leadingTab := false]].
	(alignment ~= Justified or: [leadingTab])
		ifTrue:	[self lastCharacterExtentSetX: (self nextTabXFrom: currentX) -
						currentX]
		ifFalse:	[self lastCharacterExtentSetX:  (((currentX + (self tabWidth -
						(line justifiedTabDeltaFor: spaceCount))) -
							currentX) max: 0)].
	^ true