*qualityAssistantRecording
dataForQAREntityless

	^ super dataForQAREntityless
		at: #targetMethod put: targetMethodName obfuscatedIfNecessary;
		yourself 