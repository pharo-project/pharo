querying
revparseCommit: aString 
	self withRepoDo: [ :repo | 
		^ self iceCommitFrom: (repo revparse: aString) object 
	]