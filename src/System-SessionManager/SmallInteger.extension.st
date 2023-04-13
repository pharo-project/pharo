Extension { #name : #SmallInteger }

{ #category : #'*System-SessionManager' }
SmallInteger class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 10
]
