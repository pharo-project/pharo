running
tearDown
	workingCopy unregister.
	self restoreMocks.
	self clearPackageCache.
	Author fullName: savedName.