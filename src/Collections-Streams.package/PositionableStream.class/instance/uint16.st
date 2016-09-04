accessing - data
uint16
	"Answer the next unsigned, 16-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + (self next).
	^ n
