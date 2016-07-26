bit access
nextBits: n
	| bits |
	[bitPos < n] whileTrue:[
		bitBuf := bitBuf + (self nextByte bitShift: bitPos).
		bitPos := bitPos + 8].
	bits := bitBuf bitAnd: (1 bitShift: n)-1.
	bitBuf := bitBuf bitShift: 0 - n.
	bitPos := bitPos - n.
	^bits