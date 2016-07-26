accessing
at: index put: anObject 
	^ type 
		handle: handle
		at: ((index - 1) * type typeSize) + 1
		put: anObject