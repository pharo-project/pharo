waiting-timeout
waitMaxMilliseconds: anIntegerOrNil
	"Same as Monitor>>wait, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitFor: nil maxMilliseconds: anIntegerOrNil