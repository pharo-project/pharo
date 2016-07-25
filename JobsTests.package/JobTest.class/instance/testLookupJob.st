tests
testLookupJob
	
	[ :job | 
		[ :job2 | | noneFound result |

			noneFound := false.
			result := job2 lookup: [ :aJob | aJob == job2 ] ifNone: [ noneFound := true ].
			self assert: result == job2.
			self deny: noneFound.
			
			result := job2 lookup: [ :aJob | aJob == job ] ifNone: [ noneFound := true ].
			self assert: result == job.
			self deny: noneFound.

			result := job lookup: [ :aJob | aJob == job2 ] ifNone: [ noneFound := true. 42 ].
			self assert: result = 42.
			self assert: noneFound.

			] asJob run.
	
		self assert: Job current == job.
		] asJob run