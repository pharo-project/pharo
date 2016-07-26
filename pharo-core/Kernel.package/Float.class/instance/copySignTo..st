mathematical functions
copySignTo: aNumber
	"Return a number with same magnitude as aNumber and same sign as self.
	Implementation note: take care of Float negativeZero, which is considered as having a negative sign."

	(self > 0.0 or: [(self at: 1) = 0]) ifTrue: [^ aNumber abs].
	^aNumber withNegativeSign