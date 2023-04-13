Extension { #name : #ProcessorScheduler }

{ #category : #'*System-SessionManager' }
ProcessorScheduler class >> sessionManagerInitialization [

	<kernelSessionManagerInitialization>
	SessionManager default registerSystemClassNamed: self name atPriority: 30
]
