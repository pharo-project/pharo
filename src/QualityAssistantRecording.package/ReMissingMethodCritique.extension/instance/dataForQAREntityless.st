*qualityAssistantRecording
dataForQAREntityless

	^ super dataForQAREntityless
		at: #missingSelector put: selector obfuscatedIfNecessary;
		at: #targetClass put: class dataForQAR;
		yourself 