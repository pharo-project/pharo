instance creation
imageName: anImageName
	^ self basicNew
		initializeWith: anImageName;
		yourself