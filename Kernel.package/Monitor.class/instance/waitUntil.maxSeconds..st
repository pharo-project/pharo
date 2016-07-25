waiting-timeout
waitUntil: aBlock maxSeconds: aNumber
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock maxMilliseconds: (aNumber * 1000) asInteger