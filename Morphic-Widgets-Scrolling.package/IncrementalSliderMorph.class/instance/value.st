accessing
value
	"Answer the slider value."

	^self getValueSelector
		ifNil: [(self sliderMorph ifNil: [^0]) scaledValue]
		ifNotNil: [self model
					ifNil: [(self sliderMorph ifNil: [^0]) scaledValue]
					ifNotNil: [self model perform: self getValueSelector]]