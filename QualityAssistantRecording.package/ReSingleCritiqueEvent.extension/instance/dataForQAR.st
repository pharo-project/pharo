*qualityAssistantRecording
dataForQAR

	^ super dataForQAR
		at: #critique put: critique dataForQAREntityless;
		yourself