Extension { #name : #File }

{ #category : #'*System-SessionManager' }
File class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
