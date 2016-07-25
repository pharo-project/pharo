querying
allCommits
	^ (commits ifNil: [ self loadAllCommits ]) values sorted: [ :a :b | a datetime > b datetime ]