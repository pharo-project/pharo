instance creation
newRepositoryAt: aFileReference subdirectory: aString
	^ self new
		location: aFileReference;
		subdirectory: aString;
		yourself