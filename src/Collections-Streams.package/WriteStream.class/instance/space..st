character writing
space: anInteger 
	"Append anInteger space characters to the receiver."
	
	self next: anInteger put: Character space
	