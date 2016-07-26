state tracking
setState: stateName value: aValue

	state at: stateName put: aValue.

	"receiver's state changed"
	
	self changed.