label-styles
windowLabelForText: aTextOrString
	^ (super windowLabelForText: aTextOrString)
		color: self textColor;
		yourself.