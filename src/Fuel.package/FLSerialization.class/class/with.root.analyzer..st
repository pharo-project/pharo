instance creation
with: anEncoder root: anObject analyzer: anAnalyzer
	
	^ self basicNew
		initializeWith: anEncoder
		root: anObject
		analyzer: anAnalyzer;
		yourself