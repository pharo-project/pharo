running
setUp
	"Ensure that we error, not just fail, if resource is nil so that #should:raise: checks cannot mistake such an error for what they are trapping."
	
	resource := SimpleTestResource rawCurrentForTest.
	self
		deny: resource isNil
		description: 'SimpleTestResource has no current value in test'.
	self
		assert: resource class == SimpleTestResource
		description: 'SimpleTestResource current is not an instance of itself'.
	self
		assert: resource hasSetup
		description: 'This test uses a resource but it was not set up'