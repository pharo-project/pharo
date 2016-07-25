*qualityAssistantRecording
dataForQAREntityless

	^ super dataForQAR
		at: #interval put: sourceAnchor interval dataForQAR;
		yourself 