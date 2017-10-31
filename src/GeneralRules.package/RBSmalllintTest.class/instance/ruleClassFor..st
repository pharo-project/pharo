private
ruleClassFor: aTestSelector
	^ self classNamed:
		('RB',
		(aTestSelector asString allButFirst: 4), "'test' size"
		'Rule') asSymbol.