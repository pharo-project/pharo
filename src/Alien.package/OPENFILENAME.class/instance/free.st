memory management
free
	self class primFree: (self unsignedLongAt:  self lpstrFileOffset).
	super free