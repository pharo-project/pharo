waiting-timeout
waitUntil: aBlock for: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitUntil:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger