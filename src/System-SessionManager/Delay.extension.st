Extension { #name : #Delay }

{ #category : #'*System-SessionManager' }
Delay class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 20
]
