testing-environments
testOrEnvironment
	| env1 env2 orEnvironment |
	env2 := #printOn:.
	env2 := universalEnvironment forClass: Object selectors: #(#fullPrintString).
	env1 := universalEnvironment forClass: Object selectors: #(#printOn:).
	self assert: (env1 | env2) numberSelectors = 2.
	self assert: (env2 | env1) numberSelectors = 2.
	self universalTestFor: env1 | env2.
	self assert: (env1 | env1) numberSelectors = 1.
	orEnvironment := env1 | env1 not.
	self assert: (env2 | env1) packages size = 1.
	self universalTestFor: orEnvironment.
	self assert: orEnvironment numberSelectors 
				= universalEnvironment numberSelectors.
	self assert: orEnvironment classNames asSortedCollection 
				= universalEnvironment classNames asSortedCollection.
	self assert: (orEnvironment protocolsFor: Object) 
				= ((universalEnvironment protocolsFor: Object) reject: [:each| (Object allSelectorsInProtocol: each) isEmpty ])
