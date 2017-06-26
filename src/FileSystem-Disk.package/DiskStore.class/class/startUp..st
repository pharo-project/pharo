system startup
startUp: resuming
	self checkVMVersion.
	resuming 
		ifTrue: [ self reset ]