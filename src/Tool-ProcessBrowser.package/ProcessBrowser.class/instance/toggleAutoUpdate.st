updating
toggleAutoUpdate
	self isAutoUpdating
		ifTrue: [ self stopAutoUpdate ]
		ifFalse: [ self startAutoUpdate ].
