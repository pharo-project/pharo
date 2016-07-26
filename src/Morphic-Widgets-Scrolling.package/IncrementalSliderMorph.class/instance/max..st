accessing
max: aNumber
	"Set the max value."

	(self sliderMorph ifNil: [^self]) max: aNumber