waiting-timeout
waitMaxSeconds: aNumber
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitMaxMilliseconds: (aNumber * 1000) asInteger