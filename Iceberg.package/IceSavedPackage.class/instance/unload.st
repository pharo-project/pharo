actions
unload
	self workingCopy unload.
	
	repository announcer announce: IcePackageUnloaded new.
	repository refresh