accessing
superclass
	^ superclass ifNil:[ superclassName ifNotNil: [buildEnvironment classNamed: (self superclassName)]]