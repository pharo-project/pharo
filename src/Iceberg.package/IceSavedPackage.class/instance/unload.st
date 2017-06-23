actions
unload
	self workingCopy unload.
	self repository announcer 
		announce: IcePackageUnloaded new.
	self repository refresh