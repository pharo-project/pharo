initialization
initialize
	super initialize.
	autoFlush := true.
	shouldSignal := true.
	recentlyRead := 0.
	bufferSize := 4096.
	self ascii