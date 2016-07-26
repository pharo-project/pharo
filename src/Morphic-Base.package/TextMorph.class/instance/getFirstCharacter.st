accessing
getFirstCharacter
	"obtain the first character from the receiver if it is empty, return a  
	black dot"
	| aString |
	^ (aString := text string) isEmpty
		ifTrue: ['·']
		ifFalse: [aString first asString] 