tests
testChildJob
	
	| wasRun |
	
	wasRun := false.
	
	[ :job | 
	
		[ :job2 | 
			self assert: job children size = 1.
			self assert: job children first = job2.
			wasRun := true ] asJob run.
	
		self assert: job children isEmpty ] asJob run.
	
	self assert: wasRun.