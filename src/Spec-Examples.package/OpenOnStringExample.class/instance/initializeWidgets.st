initialization
initializeWidgets
	label := self instantiate: LabelPresenter.
	check := self instantiate: CheckBoxPresenter.
	input := self instantiate: TextInputFieldPresenter.
	button := self instantiate: ButtonPresenter.
	button
		label: 'reset';
		action: [ input text: textToReset ].
	check
		label: 'Label on top';
		state: true