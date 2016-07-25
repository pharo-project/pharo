initialize-release
initializeClassVars
	(clazz isNil or: [ includeVariables not ]) ifTrue: [ ^ self ].
	classVars := clazz theNonMetaClass allClassVarNames asSortedCollection.
	classVars := classVars collect: 
		[ :each | 
		NECClassVarEntry contents: each ]