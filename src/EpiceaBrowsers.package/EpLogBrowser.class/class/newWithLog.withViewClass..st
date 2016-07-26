instance creation
newWithLog: aLog withViewClass: aViewClass

	^ self basicNew 
		initializeWithLog: aLog withViewClass: aViewClass;
		yourself