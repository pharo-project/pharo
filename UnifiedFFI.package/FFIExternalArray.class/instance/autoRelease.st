external resource management
autoRelease
	(handle isNil or: [ handle isExternalAddress not ]) ifFalse: [ ^ self ].
	handle autoRelease