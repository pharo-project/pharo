enumerating
withAllDependenciesDo: aBlock
	self allDependenciesDo: aBlock ifUnresolved: [:ignored |].
	aBlock value: self