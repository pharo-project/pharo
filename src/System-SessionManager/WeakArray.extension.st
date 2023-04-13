Extension { #name : #WeakArray }

{ #category : #'*System-SessionManager' }
WeakArray class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
