initialize-release
initializeStderr
	"install the line end conversion and initialize the converter"
	FileStream stderr wantsLineEndConversion: true; converter.
	stderr := VTermOutputDriver stderr.