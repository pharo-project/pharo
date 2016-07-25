accessing
min: aNumber
	"Set the min value."

	(self sliderMorph ifNil: [^self]) min: aNumber