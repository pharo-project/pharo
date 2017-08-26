tests
testIgnore
	| critiques |

	ReExceptionStrategy current: ReIgnoreExceptionStrategy.
	
	self shouldnt: [
			critiques := thisContext method critiques ]
		raise: MyTestError.