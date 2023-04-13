Extension { #name : #Color }

{ #category : #'*System-SessionManager' }
Color class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerGuiClassNamed: self name
]
