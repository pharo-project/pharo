Extension { #name : #ReProperty }

{ #category : #'*qualityAssistantRecording' }
ReProperty >> dataForQAR [

	^ self dataForQAREntityless
		at: #entity put: sourceAnchor sourceEntity dataForQAR;
		yourself 
]

{ #category : #'*qualityAssistantRecording' }
ReProperty >> dataForQAREntityless [

	^ super dataForQAR
		at: #interval put: sourceAnchor interval dataForQAR;
		yourself 
]
