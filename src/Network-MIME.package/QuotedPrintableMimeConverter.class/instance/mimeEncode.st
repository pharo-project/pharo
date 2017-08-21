conversion
mimeEncode
	| char charValue lineLength |
	lineLength := 0.
	[ dataStream atEnd ] whileFalse: [
		lineLength >= 74
			ifTrue: [
				mimeStream nextPut: $=; nextPutAll: newline.
				lineLength := 0].
		char := dataStream next.
		charValue := char asInteger.
		(charValue = 9 | (charValue between: 32 and: 60) | (charValue between: 62 and: 126))
			ifTrue: [
				mimeStream nextPut: char.
				lineLength := lineLength + 1 ]
			ifFalse: [
				(char = Character cr) | (char = Character lf)
					ifTrue: [
						mimeStream nextPutAll: '=0D=0A'.
						char = Character cr ifTrue: [ dataStream peekFor: Character lf ].
						lineLength := lineLength + 6 ]
					ifFalse: [
						charValue > 255 ifTrue: [ self error: 'Character out of range' ].
						mimeStream nextPut: $=.
						char asInteger printOn: mimeStream base: 16 length: 2 padded: true.
						lineLength := lineLength + 3 ] ] ]