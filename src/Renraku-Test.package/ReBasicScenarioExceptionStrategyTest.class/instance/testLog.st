tests
testLog
	| critiques |

	ReExceptionStrategy current: ReLogExceptionStrategy.
	
	self
		shouldnt: [
			critiques := self validationScenario  ]
		raise: MyTestError.
		
	self assert: critiques isNotEmpty.
	self assert: (
		critiques anySatisfy: [ :crit |
		 (crit isKindOf: ReExceptionProperty) and: [ 
			crit message includesSubstring: self errorMessage ] ])