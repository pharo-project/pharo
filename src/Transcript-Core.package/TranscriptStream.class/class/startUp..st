system startup
startUp: resuming

	Transcript ifNil: [ Smalltalk globals at: #Transcript put: self new ]