label-styles
buttonLabelForText: aTextOrString
	"Answer the label to use for the given text."
	^aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString) color: self textColor ]
		ifFalse: [super buttonLabelForText: aTextOrString]