initialization
initializeWidgets
	label := self instantiate: LabelModel.
	check := self instantiate: CheckBoxModel.
	input := self instantiate: TextInputFieldModel.
	button := self instantiate: ButtonModel.
	button
		label: 'reset';
		action: [ input text: textToReset ].
	check
		label: 'Label on top';
		state: true