updating
pauseAutoUpdate
	self isAutoUpdating
		ifTrue: [ autoUpdateProcess suspend ].
	self updateProcessList