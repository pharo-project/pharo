enumerating
allDependenciesDo: aBlock
	self allDependenciesDo: aBlock ifUnresolved: [:ignored | true]