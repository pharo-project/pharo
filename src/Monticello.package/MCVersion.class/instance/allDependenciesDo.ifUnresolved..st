enumerating
allDependenciesDo: aBlock ifUnresolved: failBlock
	| dict |
	dict := Dictionary new.
	self allDependenciesNotIn: dict do: aBlock ifUnresolved: failBlock