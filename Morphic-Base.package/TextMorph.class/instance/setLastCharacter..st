accessing
setLastCharacter: source
	"Set the last character of the receiver as indicated"

	| aChar chars |
	aChar := source asCharacter.
	(chars := self getCharacters) size > 0 
		ifFalse:
			[self newContents: (String with: aChar)]
		ifTrue:
			[(chars last = aChar) ifFalse:
				[self newContents: (String streamContents:
					[:aStream |
						aStream nextPutAll: (chars copyFrom: 1 to: (chars size - 1)).
						aStream nextPut: aChar])]]