stop conditions
cr 
	"Answer a CharacterBlock that specifies the current location of the mouse 
	relative to a carriage return stop condition that has just been 
	encountered. The ParagraphEditor convention is to denote selections by 
	CharacterBlocks, sometimes including the carriage return (cursor is at 
	the end) and sometimes not (cursor is in the middle of the text)."

	((characterIndex ~= nil
		and: [characterIndex > text size])
			or: [(line last = text size)
				and: [(destY + line lineHeight) < characterPoint y]])
		ifTrue:	["When off end of string, give data for next character"
				destY := destY +  line lineHeight.
				baselineY := line lineHeight.
				lastCharacter := nil.
				characterPoint := (nextLeftMargin ifNil: [leftMargin]) @ destY.
				(lastIndex < text size and: [(text at: lastIndex) = Character cr and: [(text at: lastIndex+1) = Character lf]])
					ifTrue: [lastIndex := lastIndex + 2]
					ifFalse: [lastIndex := lastIndex + 1].
				self lastCharacterExtentSetX: 0.
				^ true].
		lastCharacter := Character cr.
		characterPoint := destX @ destY.
		self lastCharacterExtentSetX: rightMargin - destX.
		^true