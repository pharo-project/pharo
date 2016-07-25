filenames
outputFilename
	"Answers the filename for the output file (that contains all the names and values of the constants) that the C program will write when executed"
	^  self basicGeneralFilename, '.output'