Extension { #name : #ReCritiqueEvent }

{ #category : #'*qualityAssistantRecording' }
ReCritiqueEvent >> dataForQAR [

	^ super dataForQAR
		at: #class put: self class name;
		at: #entity put: entity dataForQAR;
		yourself
]
