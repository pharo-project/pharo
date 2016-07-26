waiting-timeout
waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: [aBlock value not] for: aSymbolOrNil maxMilliseconds: anIntegerOrNil