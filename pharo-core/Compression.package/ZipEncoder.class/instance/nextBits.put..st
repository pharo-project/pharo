accessing
nextBits: nBits put: value
	"Store a value of nBits"
	"self assert:[value >= 0 and:[(1 bitShift: nBits) > value]]."
	bitBuffer := bitBuffer bitOr: (value bitShift: bitPosition).
	bitPosition := bitPosition + nBits.
	[bitPosition >= 8] whileTrue:[
		self nextBytePut: (bitBuffer bitAnd: 255).
		bitBuffer := bitBuffer bitShift: -8.
		bitPosition := bitPosition - 8].