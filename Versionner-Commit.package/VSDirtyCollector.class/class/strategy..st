instance creation
strategy: aCommitStrategy
	^ self basicNew 
		initializeStrategy: aCommitStrategy;
		yourself