private
prepareForRunning

	isRunning := true.
	process := Processor activeProcess.
	CurrentJob value ifNotNil: [ :job | job addChild: self].
	self announce: JobStart.