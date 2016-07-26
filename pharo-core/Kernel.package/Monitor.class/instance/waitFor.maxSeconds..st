waiting-timeout
waitFor: aSymbolOrNil maxSeconds: aNumber
	"Same as Monitor>>waitFor:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: aSymbolOrNil maxMilliseconds: (aNumber * 1000) asInteger