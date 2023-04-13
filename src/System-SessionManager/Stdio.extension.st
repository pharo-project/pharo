Extension { #name : #Stdio }

{ #category : #'*System-SessionManager' }
Stdio class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
