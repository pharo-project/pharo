accessing
enabled
	"Answer whether the receiver is enabled for user input."

	^self sliderMorph
		ifNil: [super enabled]
		ifNotNil: [:sm | sm enabled]