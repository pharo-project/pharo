class tests
testRoots
	| model |
	model := RBClassModelFactory rbNamespace new.
	self 
		assert: (model rootClasses asSortedCollection: [:a :b | a name < b name]) 
				asArray 
					= ((Class rootsOfTheWorld collect: [:each | model classFor: each]) 
							asSortedCollection: [:a :b | a name < b name]) asArray