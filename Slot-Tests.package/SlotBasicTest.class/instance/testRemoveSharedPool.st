tests-shared pools
testRemoveSharedPool

	self make: [ :builder | 
		builder sharedPools: 'TestSharedPool' ].

	aClass := self make: [ :builder | 
		builder sharedPools: '' ].
	
	self assert: aClass sharedPools isEmpty
