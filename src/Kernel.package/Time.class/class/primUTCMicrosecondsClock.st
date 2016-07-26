primitives
primUTCMicrosecondsClock
	"Answer the number of micro-seconds ellapsed since Squeak epoch.
	That is since 00:00 on the morning of January 1, 1901 UTC.
	At least a 60-bit unsigned integer is used internally which is enough for dates up to year 38435.
	Essential. See Object documentation whatIsAPrimitive. "

	<primitive: 240>
	self primitiveFailed