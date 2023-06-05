Extension { #name : #Symbol }

{ #category : #'*System-SessionManager' }
Symbol class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	Stdio stdout
		nextPutAll: 'Init symbol.';
		lf.
	SessionManager default registerSystemClassNamed: #Symbol
]
