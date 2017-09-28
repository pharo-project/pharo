accessing
superclass
	^ superclass ifNil:[buildEnvironment classNamed: (self superclassName)]