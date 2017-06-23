actions
addFilesToIndex: aListOfPaths
	self withRepoDo: [ :repo | 
		| index |
		index := repo index.
		index addAll: aListOfPaths.
		index writeToDisk.
	]