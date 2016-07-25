private
escapeForShell: aString
	^ aString class
		new: aString size
		streamContents: [ :stream | 
			| readStream char |
			readStream := aString readStream.
			[ readStream atEnd ]
				whileFalse: [ 
					char := readStream next.
					char = $"
						ifTrue: [ stream nextPut: $\ ].
					stream nextPut: char ] ]