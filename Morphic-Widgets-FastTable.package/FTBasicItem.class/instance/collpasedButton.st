button
collpasedButton
	^ IconicButton new
		target: self;
		actionSelector: #expandAndRefresh;
		arguments: {};
		labelGraphic: self class unexpandedForm;
		color: Color transparent;
		helpText: 'Expand Item';
		borderWidth: 0