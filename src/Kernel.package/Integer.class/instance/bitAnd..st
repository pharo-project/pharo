bit manipulation
bitAnd: n 
	"Answer an Integer whose bits are the logical AND of the receiver's bits  
	and those of the argument, n."
	| norm |
	<primitive: 'primDigitBitAnd' module:'LargeIntegers'>
	norm := n normalize.
	^ self
		digitLogic: norm
		op: #bitAnd:
		length: (self digitLength max: norm digitLength)