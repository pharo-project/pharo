accessing
cleanup
	self checkOwnerProcess.
	self critical: [self privateCleanup].