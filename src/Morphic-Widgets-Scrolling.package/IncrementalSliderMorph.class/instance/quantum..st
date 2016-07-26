accessing
quantum: aNumber
	"Set the quantum value."

	(self sliderMorph ifNil: [^self]) quantum: aNumber