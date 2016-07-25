Not categorized
resumableTestFailureTest
	self
		assert: false description: 'You should see more than me' resumable: true; 
		assert: false description: 'You should see more than me' resumable: true; 
		assert: false description: 'You should see me last' resumable: false; 
		assert: false description: 'You should not see me' resumable: true