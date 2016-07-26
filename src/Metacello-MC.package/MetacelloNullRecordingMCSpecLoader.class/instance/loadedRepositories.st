accessing
loadedRepositories

	| repos |
	repos := OrderedCollection new.
	self repositoryMap values collect: [:coll | repos addAll: coll ].
	^repos