initialization
initializeProcess
	process := [ [ self hasPermission ] whileTrue: [ self deliveryCycle ] ]
		forkAt: Processor userBackgroundPriority
		named: self printProcessName