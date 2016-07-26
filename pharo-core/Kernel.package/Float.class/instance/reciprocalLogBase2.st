mathematical functions
reciprocalLogBase2
	"optimized for self = 10, for use in conversion for printing"

	^ self = 10.0
		ifTrue: [Ln2 / Ln10]
		ifFalse: [Ln2 / self ln]