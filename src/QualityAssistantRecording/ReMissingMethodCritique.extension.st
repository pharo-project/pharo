Extension { #name : #ReMissingMethodCritique }

{ #category : #'*qualityAssistantRecording' }
ReMissingMethodCritique >> dataForQAREntityless [

	^ super dataForQAREntityless
		at: #missingSelector put: selector obfuscatedIfNecessary;
		at: #targetClass put: class dataForQAR;
		yourself 
]
