tests
testCurrentJob
	
	[ :job |
		[ :job2 |
			self assert: Job current == job2.
			self deny: Job current == job.
			] asJob run.
	
		self assert: Job current == job.
		] asJob run