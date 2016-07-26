initialize
release
	self class recycleBuffer: workBuffer.
	workBuffer := nil.
	self releaseActionMap. "we are not sure if we need it"
	super release.