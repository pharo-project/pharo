Extension { #name : #FFIBackend }

{ #category : #'*System-SessionManager' }
FFIBackend class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 60
]
