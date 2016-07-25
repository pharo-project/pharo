waiting-timeout
waitWhile: aBlock maxMilliseconds: anIntegerOrNil
	"Same as Monitor>>waitWhile:, but the process gets automatically woken up when the 
	specified time has passed."

	^ self waitWhile: aBlock for: nil maxMilliseconds: anIntegerOrNil