running
testRunTestWithResource
	self clearOuterResourceStateDuring:
		[self
			assert: (self class selector: #dummy) run printString =
							'1 run, 1 passes, 0 skipped, 0 expected failures, 0 failures, 0 errors, 0 unexpected passes'
			description: 'A dummy test that needed a resource did not pass']