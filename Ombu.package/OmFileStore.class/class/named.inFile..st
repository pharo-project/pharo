instance creation
named: aName inFile: aFileReference

	^ self basicNew 
		initializeWithGlobalName: aName
		fileReference: aFileReference;
		yourself