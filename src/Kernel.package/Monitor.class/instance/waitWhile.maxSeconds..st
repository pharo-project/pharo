waiting-timeout
waitWhile: aBlock maxSeconds: aNumber
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock maxMilliseconds: (aNumber * 1000) asInteger