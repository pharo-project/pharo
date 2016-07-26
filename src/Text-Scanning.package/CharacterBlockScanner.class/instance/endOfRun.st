stop conditions
endOfRun
	"Before arriving at the cursor location, the selection has encountered an 
	end of run. Answer false if the selection continues, true otherwise. Set 
	up indexes for building the appropriate CharacterBlock."

	| runLength lineStop |
	
	(((characterIndex ~~ nil and:
		[runStopIndex < characterIndex and: [runStopIndex < text size]])
			or:	[characterIndex == nil and: [lastIndex < line last]]) or: [
				((lastIndex < line last)
				and: [((text at: lastIndex) leadingChar ~= (text at: lastIndex+1) leadingChar)
					and: [lastIndex ~= characterIndex]])])
		ifTrue:	["We're really at the end of a real run."
				runLength := text runLengthFor: (lastIndex := lastIndex + 1).
				lineStop := characterIndex	"scanning for index"
						ifNil: [line last].		"scanning for point".
				(runStopIndex := lastIndex + (runLength - 1)) > lineStop
					ifTrue: [runStopIndex := lineStop].
				self setStopConditions.
				^false].

	self retrieveLastCharacterWidth.
		
	(characterIndex == nil and: [lastIndex = line last])
		ifTrue: [characterPoint x > (destX + (lastCharacterWidth // 2))
			ifTrue:
				[ "Correct for clicking right half of last character in line
				means selecting AFTER the char"
				lastIndex := lastIndex + 1.
				lastCharacterWidth := 0.
				characterPoint := destX + lastCharacterWidth @ destY.
				^true]].

	characterPoint := destX @ destY.
	characterIndex ~~ nil
		ifTrue:	["If scanning for an index and we've stopped on that index,
				then we back destX off by the width of the character stopped on
				(it will be pointing at the right side of the character) and return"
				runStopIndex = characterIndex
					ifTrue:	[characterPoint := destX - lastCharacterWidth @ destY.
							^true].
				"Otherwise the requested index was greater than the length of the
				string.  Return string size + 1 as index, indicate further that off the
				string by setting character to nil and the extent to 0."
				lastIndex :=  lastIndex + 1.
				lastCharacterWidth := 0.
				^true].

	"Scanning for a point and either off the end of the line or off the end of the string."
	runStopIndex = text size
		ifTrue:	["off end of string"
				lastIndex :=  lastIndex + 1.
				lastCharacterWidth := 0.
				^true].
	"just off end of line without crossing x"
	lastIndex := lastIndex + 1.
	^true