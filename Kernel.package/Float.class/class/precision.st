constants
precision
	"Answer the apparent precision of the floating point representation.
	That is the maximum number of radix-based digits (bits if radix=2) representable in floating point without round off error.
	Technically, 52 bits are stored in the representation, and normalized numbers have an implied leading 1 that does not need to be stored.
	Note that denormalized floating point numbers don't have the implied leading 1, and thus gradually loose precision.
	This format conforms IEEE 754 double precision standard."
	
	^53