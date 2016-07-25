waiting-timeout
waitFor: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitInQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.