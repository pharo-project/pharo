instance-creation
fromLongGitDescribe: aString 
	
	| parts commitish semanticVersion |
	parts := aString substrings: '-'.
	commitish := parts last allButFirst.
	semanticVersion := (parts allButLast: 2) joinUsing: '-'.
	
	^ self basicNew
		fromSemanticVersion: semanticVersion;
		commitHash: commitish;
		yourself 