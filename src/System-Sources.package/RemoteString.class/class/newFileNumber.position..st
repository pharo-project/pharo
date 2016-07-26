instance creation
newFileNumber: sourceIndex position: anInteger 
	"Answer an instance of me fora file indexed by sourceIndex, at the 
	position anInteger. Assume that the string is already stored on the file 
	and the instance will be used to access it."

	^self new fileNumber: sourceIndex position: anInteger