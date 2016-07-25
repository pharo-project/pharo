parsing-internal
parseCharacterConvertingNewLinesOn: writeStream
	| char |
	(char := readStream next) = $\
		ifTrue: [ writeStream nextPut: self parseEscapedCharacter ]
		ifFalse: [ 
			char = Character lf 
				ifTrue: [ writeStream nextPutAll: newLine ]
				ifFalse: [ 
					char = Character cr 
						ifTrue: [ 
							readStream peekFor: Character lf.
							writeStream nextPutAll: newLine ]
						ifFalse: [ writeStream nextPut: char ] ] ]