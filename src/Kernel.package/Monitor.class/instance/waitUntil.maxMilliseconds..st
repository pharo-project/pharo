waiting-timeout
waitUntil: aBlock maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitUntil:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitUntil: aBlock for: nil maxMilliseconds: anIntegerOrNil