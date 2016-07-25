bit manipulation
bitOr: n 
	"Answer an Integer whose bits are the logical OR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitOr' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitOr:
		length: (self digitLength max: norm digitLength)