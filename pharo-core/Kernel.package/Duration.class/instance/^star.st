arithmetic
* operand
	"operand is a Number"
	^ self class nanoSeconds: ( (self asNanoSeconds * operand) asInteger)
