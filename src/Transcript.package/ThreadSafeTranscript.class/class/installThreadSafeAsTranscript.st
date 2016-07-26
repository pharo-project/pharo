declare
installThreadSafeAsTranscript

	Smalltalk globals at: #Transcript put: (self new)

	"ThreadSafeTranscript open"