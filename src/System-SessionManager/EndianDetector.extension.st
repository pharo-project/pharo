Extension { #name : #EndianDetector }

{ #category : #'*System-SessionManager' }
EndianDetector class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name
]
