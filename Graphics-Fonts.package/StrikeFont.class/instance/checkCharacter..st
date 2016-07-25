testing
checkCharacter: character 
	"Answer a Character that is within the ascii range of the receiver--either 
	character or the last character in the receiver."
	| ascii |
	ascii := character asciiValue.
	(ascii < minAscii or: [ ascii > maxAscii ]) 
		ifTrue: [ ^ maxAscii asCharacter ]
		ifFalse: [ ^ character ]