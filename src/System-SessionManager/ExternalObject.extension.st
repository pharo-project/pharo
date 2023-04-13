Extension { #name : #ExternalObject }

{ #category : #'*System-SessionManager' }
ExternalObject class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 60
]
