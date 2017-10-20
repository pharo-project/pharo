tests
testOwner
	
	[ :job |
		self assert: job owner == self.
		[ :job2 |
			self assert: job owner == self.
			self assert: job2 owner == self.
 		] asJob run.
	] asJob run.
