querying
originatingCommit
	^ originatingCommit ifNil: [ originatingCommit := 
		self repository newCommitWalk 
			fromCommit: self commit;
			modifyingPackage: self package;
			firstCommit 
		]
