*qualityAssistantRecording
dataForQAR

	^ self dataForQAREntityless
		at: #entity put: sourceAnchor sourceEntity dataForQAR;
		yourself 