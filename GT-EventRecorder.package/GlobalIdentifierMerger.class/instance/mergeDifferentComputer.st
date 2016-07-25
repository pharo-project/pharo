actions
mergeDifferentComputer
	"It puts all stored values into existing dictionary."
	stored keysAndValuesDo: [ :eachKey :eachValue |
		existing at: eachKey put: eachValue ].
	self convertValues.
	^ true