class tests
testRoots
	| model |
	model := RBNamespace new.
	self 
		assert: (model rootClasses asSortedCollection: [:a :b | a name < b name]) 
				asArray 
					= ((Class rootsOfTheWorld collect: [:each | model classFor: each]) 
							asSortedCollection: [:a :b | a name < b name]) asArray