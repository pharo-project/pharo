*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #class put: self class name;
		at: #name put: self theNonMetaClass name obfuscatedIfNecessary;
		at: #isMeta put: self isMeta;
		at: #package put: self package name obfuscatedIfNecessary;
		yourself