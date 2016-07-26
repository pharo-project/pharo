label-styles
buttonLabelFor: aButton
	"Answer the label to use for the given button."

	|label|
	label := self buttonLabelForText: aButton label.
	label enabled: aButton enabled.
	label font: self buttonFont.
	label extent: label optimalExtent.
	^label