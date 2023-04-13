Extension { #name : #SharedRandom }

{ #category : #'*System-SessionManager' }
SharedRandom class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: #SharedRandom
]
