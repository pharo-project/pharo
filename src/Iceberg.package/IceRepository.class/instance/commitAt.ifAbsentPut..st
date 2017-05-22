commits
commitAt: commitId ifAbsentPut: aBlock
	^ self commitDictionary at: commitId ifAbsentPut: [ 
		| newCommit |
		newCommit := aBlock value.
		newCommit repository: self; yourself 
	]