utilities
deadServer: aStringOrNil
	"Keep the machine name of the most recently encoutered non-responding machine.  Next time the user can move it to the last in a list of servers to try."

	DeadServer := aStringOrNil