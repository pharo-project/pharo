building
tryToFillOldClass
	oldClass ifNotNil: [ ^self ].
	name ifNil: [ ^self ].
	self oldClass: (self buildEnvironment classNamed: self name)