system startup
resetAll
	"I'm reseting each shutdown and startup because like that I can be sure a complete cleanup is done, 
	 because many things can change: 
		- platform change 
		- working directory change (and then libraries not found)
		- indirect functions needs to be recalculated.
	I am not supposed to be performed when image is just saved to avoid possible problem which can happen when we reset all caches and continue to work"
	self uniqueInstance reset.
	FFICallbackFunctionResolution reset.
	FFIExternalStructure allSubclassesDo: #reset.
	uniqueInstance := nil.