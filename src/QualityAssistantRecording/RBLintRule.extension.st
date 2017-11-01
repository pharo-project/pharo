Extension { #name : #RBLintRule }

{ #category : #'*qualityAssistantRecording' }
RBLintRule >> dataForQAR [

	^ super dataForQAR
		at: #class put: self class name;
		yourself
]
