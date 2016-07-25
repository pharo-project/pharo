primitives
primMillisecondClock
	"Primitive. Answer the number of milliseconds since the millisecond clock
	 was last reset or rolled over. Answer zero if the primitive fails.
	As an alternative you can use #primUTCMillisecondsClock which does not overflow."

	<primitive: 135>
	^ 0