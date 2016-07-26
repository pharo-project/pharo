label-styles
buttonLabelForText: aTextOrString
	"Answer the label to use for the given text."
	
	^aTextOrString isString
		ifTrue: [(FuzzyLabelMorph contents: aTextOrString)
					alpha: 0.3]
		ifFalse: [super buttonLabelForText: aTextOrString]