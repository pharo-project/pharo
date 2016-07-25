tests
testIsDevelopmentUsedInRelease
	| workflow |
	workflow := project devWorkflow.
	self assert: workflow isDevelopmentUsedInRelease equals: false.
	
	self setDevelopment: '1.0-baseline'.
	self assert: workflow isDevelopmentUsedInRelease equals: true.
	
	self setDevelopment: '2.0-baseline'.
	self assert: workflow isDevelopmentUsedInRelease equals: false.
	
	self setDevelopment: '1.1'.
	self assert: workflow isDevelopmentUsedInRelease equals: false.
