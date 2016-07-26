state tracking
resetState: stateName

	state at: stateName put: nil.

	"receiver's state changed"
	
	self changed.