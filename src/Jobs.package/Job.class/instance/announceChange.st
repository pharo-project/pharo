accessing
announceChange

	isRunning ifFalse: [ ^ self ].
	self announce: JobChange.