querying
contentOfFileNamed: aPathString commit: aCommitId
	^ self backend 
		contentOfFileNamed: aPathString
		commit: aCommitId