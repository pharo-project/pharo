deactivation
deactivate
	^ mutex critical: [ | oldProcess |
		self withdrawPermission.
		delay ifNotNil: [ :aDelay | aDelay delaySemaphore signal ].
		oldProcess := process.
		process := nil.
		oldProcess ]