querying
allDependenciesIn: aCollection
	^ aCollection detect: [ :aTrait | aTrait traitName = self value ] ifFound: [ :x | {x} ] ifNone: [ #() ]