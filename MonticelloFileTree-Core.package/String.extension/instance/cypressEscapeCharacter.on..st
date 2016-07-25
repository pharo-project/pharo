*monticellofiletree-core
cypressEscapeCharacter: aCharacter on: aStream

	^ ('\"''' includes: aCharacter)
		ifTrue: [ 
			aStream 
				nextPut: $\;
				nextPut: aCharacter ] 
		ifFalse: [ 
			aStream nextPut: aCharacter ]