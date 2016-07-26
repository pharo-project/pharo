unregistering
unregister: aWorkingCopy
	self unregisterWorkingCopy: aWorkingCopy.
	self unregisterRepositories: aWorkingCopy.
	self unregisterPackageSet: aWorkingCopy.
	self unregisterPackageInfo: aWorkingCopy