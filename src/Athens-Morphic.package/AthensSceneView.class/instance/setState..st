state tracking
setState: stateName

	state at: stateName put: true.

	"receiver's state changed"
	
	self changed.