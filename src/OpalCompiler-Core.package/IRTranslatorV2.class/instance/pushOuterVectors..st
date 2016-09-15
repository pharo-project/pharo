results
pushOuterVectors: scope
	| scopesWithVector sc |
	scopesWithVector := OrderedCollection new.
	sc := scope.
	[ sc outerScope isNil ] whileFalse: [ 
		sc := sc outerScope.
		sc hasTempVector ifTrue: [ scopesWithVector add: sc ] ].
	scopesWithVector reverseDo: [ :scopeWithVector |
		tempVectorStack push: (IRInstruction 
			createTempVectorNamed: scopeWithVector tempVectorName
			withVars: (scopeWithVector tempVector collect: [:each| each name]) asArray ) ].
	gen inBlock: true.