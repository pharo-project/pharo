private
dummy
	self assert: resource hasSetup
		description: 'This test uses a resource but it was not set up'.
	self setRun.
	self assert: resource hasRun
		description: 'This test uses a resource but we could not interact with it'.