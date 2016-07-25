generating C program
generateCProgram
	"This method is the one that finally autogenereates the C program"
	| stream | 
	stream := StandardFileStream forceNewFileNamed: self cProgramFilename "'/Users/mariano/prueba.c'".
	self writeCMainCommentOn: stream.
	self writeCIncludesOn: stream.
	self writeRestOfCFileOn: stream.
	stream flush; close.