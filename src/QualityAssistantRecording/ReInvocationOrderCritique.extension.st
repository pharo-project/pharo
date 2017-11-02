Extension { #name : #ReInvocationOrderCritique }

{ #category : #'*qualityAssistantRecording' }
ReInvocationOrderCritique >> dataForQAREntityless [

	^ super dataForQAREntityless
		at: #targetMethod put: targetMethodName obfuscatedIfNecessary;
		yourself 
]
