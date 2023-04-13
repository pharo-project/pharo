Extension { #name : #BasicCommandLineHandler }

{ #category : #'*System-SessionManager' }
BasicCommandLineHandler class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
