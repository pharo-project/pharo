waiting-timeout
waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	self checkOwnerProcess.
	self waitWhile: aBlock inQueue: (self queueFor: aSymbolOrNil) maxMilliseconds: anIntegerOrNil.