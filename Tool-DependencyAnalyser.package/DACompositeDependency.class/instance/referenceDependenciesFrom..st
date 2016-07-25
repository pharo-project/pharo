accessing
referenceDependenciesFrom: aClass
	| references |
	references := self referenceDependencies.
	
	^ references select: [ :reference | reference theClass = aClass ].