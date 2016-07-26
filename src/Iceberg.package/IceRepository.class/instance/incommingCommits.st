querying
incommingCommits
	| loadedCommits |
	loadedCommits := self loadedPackages 
		collect: [ :package | package loadedVersion commitId ].
	
	^ self allCommitsNewerThanAny: loadedCommits	