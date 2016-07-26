special context creation
theReturnMethod

	| meth |
	meth := self lookupSelector: #return:.
	meth isPrimitive ifTrue: [^ self error: 'expected #return: to not be a primitive'].
	^ meth