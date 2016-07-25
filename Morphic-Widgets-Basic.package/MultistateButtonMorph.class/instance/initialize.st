initialization
initialize
	"Initialize the receiver."

	self stateMap: KeyedTree new.
	enabled := true.
	active := true.
	over := false.
	down := false.
	super initialize