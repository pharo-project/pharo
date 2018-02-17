Extension { #name : #TraitDescription }

{ #category : #'*qualityAssistantRecording' }
TraitDescription >> dataForQAR [

	^ super dataForQAR
		at: #class put: self class name;
		at: #name put: self instanceSide name obfuscatedIfNecessary;
		at: #isMeta put: self isMeta;
		at: #package put: self package name obfuscatedIfNecessary;
		yourself
]
