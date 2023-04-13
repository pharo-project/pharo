Extension { #name : #UUIDGenerator }

{ #category : #'*System-SessionManager' }
UUIDGenerator class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerNetworkClassNamed: self name
]
