Extension { #name : #OSPlatform }

{ #category : #'*System-SessionManager' }
OSPlatform class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 50
]
