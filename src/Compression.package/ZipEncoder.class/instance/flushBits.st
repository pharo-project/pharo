flushing
flushBits
	"Flush currently unsent bits"
	[bitPosition > 0] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].
	bitPosition := 0.