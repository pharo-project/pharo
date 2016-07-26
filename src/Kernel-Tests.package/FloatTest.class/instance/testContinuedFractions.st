tests - arithmetic
testContinuedFractions
	self assert: (Float pi asApproximateFractionAtOrder: 1) = (22/7).
	self assert: (Float pi asApproximateFractionAtOrder: 3) = (355/113)