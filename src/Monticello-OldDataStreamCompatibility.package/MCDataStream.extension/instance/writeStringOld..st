*Monticello-OldDataStreamCompatibility
writeStringOld: aString
	"PRIVATE -- Write the contents of a String."

	| length |
	aString size < 16384 
		ifTrue: [
			(length := aString size) < 192
				ifTrue: [byteStream nextPut: length]
				ifFalse: 
					[byteStream nextPut: (length // 256 + 192).
					byteStream nextPut: (length \\ 256)].
			aString do: [:char | byteStream nextPut: char asciiValue]]
		ifFalse: [self writeByteArray: aString].	"takes more space"