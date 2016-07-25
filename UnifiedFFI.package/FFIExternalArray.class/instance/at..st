accessing
at: index 
	^ type 
		handle: handle
		at: ((index - 1) * type typeSize) + 1