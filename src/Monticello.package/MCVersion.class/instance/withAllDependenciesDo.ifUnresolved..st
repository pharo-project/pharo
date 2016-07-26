enumerating
withAllDependenciesDo: aBlock ifUnresolved: failBlock
	| dict |
	dict := Dictionary new.
	self allDependenciesNotIn: dict do: aBlock ifUnresolved: failBlock.
	aBlock value: self