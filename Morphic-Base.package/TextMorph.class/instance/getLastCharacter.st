accessing
getLastCharacter
	"obtain the last character from the receiver if it is empty, return a black dot"

	| aString |
	^ (aString := text string) size > 0 ifTrue: [aString last asString] ifFalse: ['·']