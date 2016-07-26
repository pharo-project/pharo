running
signalInitializationError
	^ self classForTestResult signalFailureWith:  'Resource ' , self name , ' could not be initialized'