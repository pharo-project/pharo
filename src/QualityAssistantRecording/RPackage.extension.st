Extension { #name : #RPackage }

{ #category : #'*qualityAssistantRecording' }
RPackage >> dataForQAR [

	^ super dataForQAR
		at: #class put: self class name;
		at: #name put: self name obfuscatedIfNecessary;
		yourself
]
