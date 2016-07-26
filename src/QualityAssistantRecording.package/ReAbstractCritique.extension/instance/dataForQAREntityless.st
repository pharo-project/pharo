*qualityAssistantRecording
dataForQAREntityless

	^ super dataForQAREntityless
		at: #rule put: rule dataForQAR;
		yourself