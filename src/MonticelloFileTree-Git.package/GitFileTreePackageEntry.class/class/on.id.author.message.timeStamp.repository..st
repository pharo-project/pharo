instance creation
on: aPackageDirectory id: aCommitID author: aString message: aMessage timeStamp: aDateAndTime repository: aRepository
	^ self new
		packageDirectory: aPackageDirectory;
		commitID: aCommitID;
		author: aString;
		message: aMessage;
		timeStamp: aDateAndTime;
		repository: aRepository