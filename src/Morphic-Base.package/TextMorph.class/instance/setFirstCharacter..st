accessing
setFirstCharacter: source 
	"Set the first character of the receiver as indicated"
	| aChar chars |
	aChar := source asCharacter.
	(chars := self getCharacters) isEmpty
		ifTrue: [self
				newContents: (String with: aChar)]
		ifFalse: [chars first = aChar
				ifFalse: [self
						newContents: (String
								streamContents: [:aStream | 
									aStream nextPut: aChar.
									aStream
										nextPutAll: (chars copyFrom: 2 to: chars size)])]] 