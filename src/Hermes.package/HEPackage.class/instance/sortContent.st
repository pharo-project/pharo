adding
sortContent
	| allClasses|
	traits := traits sorted: [ :a :b | (a traitComposition allDependenciesIn: traits) size < (b traitComposition allDependenciesIn: traits) size ].
	
	allClasses := classes.
	classes := classes sorted: [ :a :b | (self numberOfRequiredClassesOf: a in: allClasses) < (self numberOfRequiredClassesOf: b in: allClasses) ].
