instance creation
oldClass: anOldClass newClass: aNewClass

	^ self basicNew
		initializeOldClass: anOldClass
		newClass: aNewClass;
		yourself
