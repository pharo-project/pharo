initialization
initializeHandle: aHandle offset: aNumber
	self initialize.
	handle := aHandle.
	startOffset := aNumber - 1 "starting offset is zero based"