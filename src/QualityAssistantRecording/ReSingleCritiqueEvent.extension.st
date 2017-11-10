Extension { #name : #ReSingleCritiqueEvent }

{ #category : #'*qualityAssistantRecording' }
ReSingleCritiqueEvent >> dataForQAR [

	^ super dataForQAR
		at: #critique put: critique dataForQAREntityless;
		yourself
]
