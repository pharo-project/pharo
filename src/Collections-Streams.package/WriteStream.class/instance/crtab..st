character writing
crtab: anInteger 
	"Append a return character, followed by anInteger tab characters, to the 
	receiver."

	self nextPut: Character cr.
	anInteger timesRepeat: [self nextPut: Character tab]