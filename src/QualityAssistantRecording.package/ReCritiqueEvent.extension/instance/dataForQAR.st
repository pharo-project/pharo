*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #class put: self class name;
		at: #entity put: entity dataForQAR;
		yourself