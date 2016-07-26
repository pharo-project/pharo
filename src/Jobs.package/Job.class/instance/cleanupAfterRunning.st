private
cleanupAfterRunning
	isRunning := false.
	process := nil.
	self announce: JobEnd.
	parent ifNotNil: [ :job | job removeChild: self ].