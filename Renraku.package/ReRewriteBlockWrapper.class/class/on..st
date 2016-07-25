instance creation
on: aBlock

	^ self basicNew
		initializeBlock: aBlock;
		yourself