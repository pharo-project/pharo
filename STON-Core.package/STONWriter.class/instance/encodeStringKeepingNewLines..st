private
encodeStringKeepingNewLines: string
	| input char |
	input := string readStream.
	[ input atEnd ]
		whileFalse: [ 
			char := input next.
			char = Character lf
				ifTrue: [ writeStream nextPutAll: newLine ]
				ifFalse: [ 
					char = Character cr
						ifTrue: [ 
							input peekFor: Character lf.
							writeStream nextPutAll: newLine ]
						ifFalse: [ self encodeCharacter: char ] ] ]