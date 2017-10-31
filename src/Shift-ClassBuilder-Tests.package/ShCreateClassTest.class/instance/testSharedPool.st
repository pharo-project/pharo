tests
testSharedPool
	builder name: #SHClassWithSharedPool.
	result := builder
		sharedPools: 'ShTestSharedPool';
		build.

	self validateResult.
	self validateSuperclass: Object.
	self validateSharedPools: {ShTestSharedPool}