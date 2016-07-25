conversion
nextFromStream: aStream

	| char resultChar |
	char := super nextFromStream: aStream.
	(CombinedChar isCompositionCharacter: char charCode)
		ifFalse: [^char].
	combinedChar
		ifNil: [
			combinedChar := CombinedChar new.
			combinedChar simpleAdd: char]
		ifNotNil: [
			[combinedChar simpleAdd: char]
				whileFalse: [char := super nextFromStream: aStream].
			resultChar := combinedChar combined.
			combinedChar := CombinedChar new.
			combinedChar simpleAdd: char.
			^resultChar]
