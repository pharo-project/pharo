*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #banLevel put: entityBanLevel dataForQAR;
		yourself