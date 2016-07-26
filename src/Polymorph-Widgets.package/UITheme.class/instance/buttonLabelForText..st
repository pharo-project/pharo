label-styles
buttonLabelForText: aTextOrString
	"Answer the label to use for the given text."

	^aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString)
					disabledStyle: self disabledItemStyle]
		ifFalse: [|t|
				t := TextMorph new newContents: aTextOrString.
				t
					extent: t optimalExtent;
					margins: (0@0 corner: 0@1);
					lock]