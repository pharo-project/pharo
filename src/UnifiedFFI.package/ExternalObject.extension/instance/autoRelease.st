*UnifiedFFI
autoRelease
	self isExternalAddress ifFalse: [ ^ self ].
	^ self getHandle autoRelease