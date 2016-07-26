iterating
dependenciesAndKnownDependantsDo: aBlockClosure 
	dependencies keysAndValuesDo: [ :pkgName :deps |
		aBlockClosure 
			value: pkgName
			value: deps
			value: (self knownDependantsOf: pkgName) ]