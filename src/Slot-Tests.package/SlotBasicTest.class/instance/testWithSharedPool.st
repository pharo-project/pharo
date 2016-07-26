tests-shared pools
testWithSharedPool

	aClass := self make: [ :builder | 
		builder sharedPools: 'TestSharedPool' ].
	aClass class compile: 'one ^One'.
	
	self assert: (aClass sharedPools includes: TestSharedPool).
	self assert: aClass one = 1.