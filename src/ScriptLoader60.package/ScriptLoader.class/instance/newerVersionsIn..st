load primitives
newerVersionsIn: aCollection
	| possiblyNewer |
	possiblyNewer := aCollection reject: [ :version |
		self allManagers anySatisfy: [ :workingCopy |
		workingCopy ancestry ancestorString , '.mcz' = version]].
	" reject unloaded packages "
	^ possiblyNewer select: [ :version |
		| packageName |
		packageName := (version copyAfter: $-) reverse.
		self allManagers anySatisfy: [ :workingCopy |
			workingCopy package name = packageName ] ]