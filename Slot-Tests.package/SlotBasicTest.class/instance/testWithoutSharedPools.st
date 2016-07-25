tests-shared pools
testWithoutSharedPools

	aClass := self make: [ :builder | 
		builder sharedPools: '' ].
	
	self assert: aClass sharedPools isEmpty