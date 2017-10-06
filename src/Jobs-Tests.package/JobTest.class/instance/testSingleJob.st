tests
testSingleJob

	| wasRun |
	wasRun := false.
	[ :job | 
		self assert: job children isEmpty.
		wasRun := true ] asJob run.	
	self assert: wasRun.