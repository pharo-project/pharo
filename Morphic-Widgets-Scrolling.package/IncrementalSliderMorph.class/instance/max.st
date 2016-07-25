accessing
max
	"Answer the max value."

	^(self sliderMorph ifNil: [^0]) max