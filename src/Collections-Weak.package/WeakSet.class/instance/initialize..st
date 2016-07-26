initialization
initialize: n
	"Initialize array to an array size of n"

	flag := Object new.
	array := WeakArray new: n.
	array atAllPut: flag.
	tally := 0