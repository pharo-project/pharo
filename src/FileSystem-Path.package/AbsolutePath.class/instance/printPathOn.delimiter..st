printing
printPathOn: aStream delimiter: aCharacter
	"Print the path elements of the receiver, without the 'Path *' part"

	aStream nextPut: aCharacter.
	super printPathOn: aStream delimiter: aCharacter
