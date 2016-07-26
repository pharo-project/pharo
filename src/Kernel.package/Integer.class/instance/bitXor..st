bit manipulation
bitXor: n 
	"Answer an Integer whose bits are the logical XOR of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitXor' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitXor:
		length: (self digitLength max: norm digitLength)