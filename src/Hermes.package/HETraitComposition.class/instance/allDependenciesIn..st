querying
allDependenciesIn: aCollection
	^ self value flatCollect: [ :aTrait | aTrait allDependenciesIn: aCollection ]