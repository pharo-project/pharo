tests-shared pools
testAddSharedPool

	self make: [ :builder | 
		builder sharedPools: '' ].

	aClass := self make: [ :builder | 
		builder sharedPools: 'TestSharedPool' ].
	
	self assert: (aClass sharedPools includes: TestSharedPool).
