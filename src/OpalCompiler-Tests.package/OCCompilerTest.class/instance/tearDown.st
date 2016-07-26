running
tearDown
	Smalltalk globals at: #Transcript put: originalTranscript.
	Smalltalk globals removeKey: #OCCompilerTestTestVar