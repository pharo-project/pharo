accessing
stdout
	
	"install the line end conversion and initialize the converter"
	 FileStream stdout wantsLineEndConversion: true; converter.
	
	^ FileStream stdout