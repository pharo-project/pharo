repositories
repositoriesFrom: aMetacelloMVRepositorySpecs ignoreOverrides: ignoreOverrides

	| repositories repos |
	(ignoreOverrides not and: [self hasRepositoryOverrides]) ifTrue: [ ^self loaderPolicy overrideRepositories ].
	repositories := MCRepositoryGroup default repositories.
	repos := OrderedCollection new.
	aMetacelloMVRepositorySpecs do: [:aSpec | | description repo |
		description := aSpec description.
		(repo := repositories detect: [:rep | rep description = description ] ifNone: [ aSpec createRepository ]) ~~ nil
			ifTrue: [ repos add: repo ]].
	^repos