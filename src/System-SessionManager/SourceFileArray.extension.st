Extension { #name : #SourceFileArray }

{ #category : #'*System-SessionManager' }
SourceFileArray class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
