merging
mergeSpec: anotherSpec

	| newSpec map anotherPackages anotherRepositories |
	newSpec := super mergeSpec: anotherSpec.
	map := anotherSpec mergeMap.
	(anotherRepositories := map at: #repositories) notEmpty
		ifTrue: [ 
			newSpec 
				repositories: (self repositories isEmpty
					ifTrue: [ anotherRepositories ]
					ifFalse: [ self repositories mergeSpec: anotherRepositories ]) ].
	^newSpec