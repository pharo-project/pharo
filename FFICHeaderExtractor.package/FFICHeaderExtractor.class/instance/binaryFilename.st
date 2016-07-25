filenames
binaryFilename
	"Answers the filename for the compiled C program, that is, the binary file. This is typically the argument to the '-o' in gcc and cc."
	^ self basicGeneralFilename , '.o'