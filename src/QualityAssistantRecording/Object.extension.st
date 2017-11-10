Extension { #name : #Object }

{ #category : #'*qualityAssistantRecording' }
Object >> dataForQAR [

	^ Dictionary newFromPairs: {
		#class . self class name obfuscatedIfNecessary }
]

{ #category : #'*qualityAssistantRecording' }
Object >> obfuscatedIfNecessary [

	^ GlobalIdentifier uniqueInstance obfuscateIfNecessary: self
]
