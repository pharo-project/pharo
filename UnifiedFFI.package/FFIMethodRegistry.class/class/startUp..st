system startup
startUp: resuming
	resuming ifFalse: [ ^ self ].
	"I'm reseting each startup because like that I can be sure a complete cleanup is done, 
	 because many things can change: 
		- platform change 
		- working directory change (and then libraries not found)
		- indirect functions needs to be recalculated"
	self uniqueInstance reset.
	FFICallbackFunctionResolution reset.
