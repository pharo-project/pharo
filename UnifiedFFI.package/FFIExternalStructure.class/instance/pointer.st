converting
pointer
	self getHandle isExternalAddress 
		ifFalse: [ self error: 'Structures need to be moved to external memory space before passing them as pointers.' ].
	^ self getHandle pointer