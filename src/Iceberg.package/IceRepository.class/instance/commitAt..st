commits
commitAt: commitId
	commitId size = 40 
		ifTrue: [ 
			^ self commitDictionary 
				at: commitId
				ifAbsent: [ self lookupCommit: commitId ]].

	"Commit id is an abbreviation, sequential search"	
	(self commitDictionary keys 
		select: [ :key | key beginsWith: commitId ]) 
		in: [ :possibleKeys | 
			possibleKeys size = 1 
				ifTrue: [ ^ self commitDictionary at: possibleKeys anyOne ] 
				ifFalse: [ possibleKeys isEmpty 
					ifTrue: [ ^ self lookupCommit: commitId ] 
					ifFalse: [ self error: 'Ambiguous commit hexString: ', commitId  ]]
		]
