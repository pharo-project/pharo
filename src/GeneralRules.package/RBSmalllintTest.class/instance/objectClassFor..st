private
objectClassFor: aTestSelector
	^ self classNamed:
		('RBSmalllintTest',
		(aTestSelector asString allButFirst: 4 "'test' size"),
		'Object') asSymbol