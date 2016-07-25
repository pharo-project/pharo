label-styles
checkboxLabelForText: aTextOrString
	"Answer the label to use for the given text."

	|morph|
	morph := aTextOrString isString
		ifTrue: [(LabelMorph contents: aTextOrString)
				disabledStyle: self disabledItemStyle]
		ifFalse: [|t|
				t := TextMorph new newContents: aTextOrString.
				t extent: t optimalExtent].
	^morph 
		vResizing: #shrinkWrap;
		hResizing: #spaceFill;
		lock