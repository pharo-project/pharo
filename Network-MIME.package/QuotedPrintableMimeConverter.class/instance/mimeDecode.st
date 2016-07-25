conversion
mimeDecode
	| char outChar previousChar |
	previousChar := nil.
	[ mimeStream atEnd ] whileFalse: [
		char := mimeStream next.
		outChar := (char = $=
			ifTrue: [
				((char := mimeStream next) = Character cr) | (char = Character lf)
					ifTrue: [
						char = Character cr ifTrue: [ mimeStream peekFor: Character lf ].
						nil ]
					ifFalse: [
						Character codePoint: char digitValue * 16 + mimeStream next digitValue ] ]
			ifFalse: [ char ]).
		outChar notNil 
			ifTrue: [ 
				(outChar = Character lf) & (previousChar = Character cr)
					ifTrue: [ dataStream nextPutAll: newline ]
					ifFalse: [ 
						(outChar = Character cr) | (outChar = Character lf) 
							ifFalse: [ dataStream nextPut: outChar ] ].
				previousChar := outChar ] ]