system startup
startUp: resuming
	self checkVMVersion.
	resuming 
		ifTrue: [ self reset ].
	DefaultWorkingDirectory := self defaultWorkingDirectory.