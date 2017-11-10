Extension { #name : #ReCritiqueBanned }

{ #category : #'*qualityAssistantRecording' }
ReCritiqueBanned >> dataForQAR [

	^ super dataForQAR
		at: #banLevel put: entityBanLevel dataForQAR;
		yourself
]
