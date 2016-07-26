converting
privateAsArray

	self resetArrayComputation.
	self countNumberOfProportionals.

	autoFractionComputation
		ifTrue: [ self autoFractionComputation ]
		ifFalse: [ self manualFractionComputation ].

	self shiftLastWidgets.

	result addAll: super privateAsArray.
	^ result asArray