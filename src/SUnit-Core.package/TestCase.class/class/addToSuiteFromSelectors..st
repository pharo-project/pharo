building suites
addToSuiteFromSelectors: suite
	^self addToSuite: suite fromMethods: (self shouldInheritSelectors
		ifTrue: [ self allTestSelectors ]
		ifFalse: [self testSelectors ])