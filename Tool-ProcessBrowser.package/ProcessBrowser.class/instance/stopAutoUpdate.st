updating
stopAutoUpdate
	autoUpdateProcess ifNotNil: [
		autoUpdateProcess terminate.
		autoUpdateProcess := nil].
	self updateProcessList