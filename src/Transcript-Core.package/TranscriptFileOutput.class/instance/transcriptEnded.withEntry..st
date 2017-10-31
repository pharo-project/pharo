transcript
transcriptEnded: aTranscript withEntry: aString

	"The transcript could be closed due to the image being closed and reopened"
	self hasValidStream ifFalse: [ self initializeStream ].
	
	fileStream nextPutAll: aString