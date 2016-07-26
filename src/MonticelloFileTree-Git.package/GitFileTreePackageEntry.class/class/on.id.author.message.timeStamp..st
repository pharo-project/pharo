instance creation
on: aPackageDirectory id: aCommitID author: aString message: aMessage timeStamp: aDateAndTime
	^ self new
		packageDirectory: aPackageDirectory;
		commitID: aCommitID;
		author: aString;
		message: aMessage;
		timeStamp: aDateAndTime