instance creation
on: aCommitOrTree iceVersion: iceVersion
	^ (self on: aCommitOrTree) 
		iceVersion: iceVersion; 
		yourself