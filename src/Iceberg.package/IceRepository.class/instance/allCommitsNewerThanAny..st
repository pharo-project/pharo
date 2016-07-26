querying
allCommitsNewerThanAny: commitIds
	| incommingCommits |
	incommingCommits := OrderedCollection new.
	
	self allCommits do: [ :commit | 
		(commitIds includes: commit commitId )
			ifTrue: [ ^ incommingCommits ]
			ifFalse: [incommingCommits add: commit]].
		
	^ incommingCommits.
	