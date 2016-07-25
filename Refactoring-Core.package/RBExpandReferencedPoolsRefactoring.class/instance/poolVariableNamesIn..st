transforming
poolVariableNamesIn: poolName 
	^(Smalltalk globals at: poolName ifAbsent: [Dictionary new]) classPool keys 
		collect: [:name | name asString]