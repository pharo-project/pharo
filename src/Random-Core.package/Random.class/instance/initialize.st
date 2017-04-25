initialization
initialize
	self useClockBasedSeed.
	a := 16r000041A7 asFloat.    " magic constant =      16807 = 7^5"
	m := 16r7FFFFFFF asFloat.    " magic constant = 2147483647 = 2^31 - 1"
	q := (m quo: a) asFloat.
	r  := (m \\ a) asFloat.
