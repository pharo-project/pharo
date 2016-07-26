initialize-release
initializeStdout
	"install the line end conversion and initialize the converter"
	FileStream stdout wantsLineEndConversion: true; converter.
	stdout := VTermOutputDriver stdout.