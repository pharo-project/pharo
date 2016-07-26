converting
privateAsArray

	self resetArrayComputation.
	self countNumberOfProportionals.
	
	autoFractionComputation
		ifTrue: [ self manualFractionComputation ]
		ifFalse: [ self autoFractionComputation ].
	
	self shiftLastWidgets.
	
	result addAll: super privateAsArray.
	^ result asArray