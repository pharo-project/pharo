instance creation
newAnalyzing: anAnalysis references: aCollection

	^ self basicNew
		initializeWithClass: anAnalysis references: aCollection;
		initializeAnalyzing;
		yourself