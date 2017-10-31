class initialization
initialize
	"Make sure to cleanup on shutdown"
	(Transcript isNil or: [ Transcript class ~~ self ]) ifTrue: [ 
		Smalltalk globals at: #Transcript put: (self new)
	].
	
	SessionManager default registerSystemClassNamed: self name.
	