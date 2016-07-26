scripting access
getAllButFirstCharacter
	"Obtain all but the first character from the receiver; if that would be empty, return a black dot"

	| aString |
	^ (aString := text string) size > 1 ifTrue: [aString copyFrom: 2 to: aString size] ifFalse: ['·']