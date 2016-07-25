tests - set arithmetic
containsAll: union of: one andOf: another
			
	self assert: (one allSatisfy: [:each | union includes: each]).
	self assert: (another allSatisfy: [:each | union includes: each])