waiting-timeout
waitWhile: aBlock for: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitWhile:for:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger