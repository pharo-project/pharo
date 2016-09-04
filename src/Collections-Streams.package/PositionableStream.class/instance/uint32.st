accessing - data
uint32
	"Answer the next unsigned, 32-bit integer from this (binary) stream."

	| n |
	n := self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	n := (n bitShift: 8) + self next.
	^ n
