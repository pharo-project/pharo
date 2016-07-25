instance creation
onFileNamed: fileName
	"Create a NonInteractiveTranscript that will write to fileName."
	
	^ self new
		fileName: fileName;
		yourself