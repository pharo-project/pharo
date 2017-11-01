Extension { #name : #ReAbstractRule }

{ #category : #'*qualityAssistantRecording' }
ReAbstractRule >> dataForQAR [

	^ super dataForQAR
		at: #class put: self class name;
		yourself
]
