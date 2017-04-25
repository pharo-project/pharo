system startup
shutDown: quitting
	quitting ifFalse: [ ^ self ].
	"I'm reseting each shutdown because like that I can be sure a complete cleanup is done, 
	 because many things can change: 
		- platform change 
		- working directory change (and then libraries not found)
		- indirect functions needs to be recalculated"
	self uniqueInstance reset.
	FFICallbackFunctionResolution reset.
	FFIExternalStructure allSubclassesDo: #reset
