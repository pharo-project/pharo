tests
testIgnore
	| critiques |

	ReExceptionStrategy current: ReIgnoreExceptionStrategy.
	
	self
		shouldnt: [
			critiques := self validationScenario  ]
		raise: MyTestError.
		
	self assert: critiques isEmpty