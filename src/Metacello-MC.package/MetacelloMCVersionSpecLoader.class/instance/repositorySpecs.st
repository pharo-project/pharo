spec compatibility
repositorySpecs

	| repositoryMap |
	repositoryMap := self versionSpec repositories ~~ nil
		ifTrue: [ self versionSpec repositories map ]
		ifFalse: [ Dictionary new ].
	^repositoryMap values.