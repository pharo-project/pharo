running
setUp
	originalTranscript := Transcript.
	newTranscript := MockTranscript new.
	Smalltalk globals at: #Transcript put: newTranscript.
	Smalltalk globals at: #OCCompilerTestTestVar put: MockForCompilation.
	
	