instance creation
newWithBrowser: aLogBrowser 
	
	^ self basicNew
		initializeWithBrowser: aLogBrowser;
		yourself