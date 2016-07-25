*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #preConditions put: (preConditions collect: #dataForQAR);
		at: #postConditions put: (postConditions collect: #dataForQAR);
		yourself