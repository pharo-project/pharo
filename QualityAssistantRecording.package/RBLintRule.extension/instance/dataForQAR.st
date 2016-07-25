*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #class put: self class name;
		yourself