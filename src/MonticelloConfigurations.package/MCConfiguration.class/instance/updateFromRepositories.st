updating
updateFromRepositories
	| oldInfos newNames sortedNames newDeps |
	oldInfos := self dependencies collect: [:dep | dep versionInfo].
	newNames := Dictionary new.
	self repositories
		do: [ :repo | 
			ProgressNotification signal: '' extra: 'Checking ', repo description.
			(repo possiblyNewerVersionsOfAnyOf: oldInfos)
				do: [:newName | newNames at: newName put: repo]]
		displayingProgress: 'Searching new versions'.

	sortedNames := newNames keys asSortedCollection:
		[:a :b | a numericSuffix > b numericSuffix].

	newDeps := OrderedCollection new.
	self dependencies do: [:dep |
		| newName |
		newName := sortedNames
			detect: [:each | (each copyUpToLast: $-) = dep package name]
			ifNone: [nil].
		newDeps add: (newName
			ifNil: [dep]
			ifNotNil: [
				| repo ver  |
				repo := newNames at: newName.
				ver := self versionNamed: newName for: dep from: repo.
				ver ifNil: [dep]
					ifNotNil: [MCVersionDependency package: ver package info: ver info]
			])
	] displayingProgress: 'downloading new versions'.

	self dependencies: newDeps.
