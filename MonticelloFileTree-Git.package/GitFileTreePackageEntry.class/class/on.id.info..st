instance creation
on: aPackageDirectory id: aCommitID info: info
	^ self new
		packageDirectory: aPackageDirectory;
		info: info;
		commitID: aCommitID