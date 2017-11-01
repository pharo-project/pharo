Extension { #name : #ReAbstractCritique }

{ #category : #'*qualityAssistantRecording' }
ReAbstractCritique >> dataForQAREntityless [

	^ super dataForQAREntityless
		at: #rule put: rule dataForQAR;
		yourself
]
