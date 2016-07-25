accessing
stderr
	
	"install the line end conversion and initialize the converter"
	 FileStream stderr wantsLineEndConversion: true; converter.
	
	^ FileStream stderr