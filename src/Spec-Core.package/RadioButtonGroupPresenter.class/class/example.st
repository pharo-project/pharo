example
example
	| example |
	example := RadioButtonGroupPresenter new.
	example
		addRadioButton: (RadioButtonPresenter new label: '1');
		addRadioButton: (RadioButtonPresenter new label: '2');
		addRadioButton: (RadioButtonPresenter new label: '3');
		whenCurrentActivatedChanged: [ self inform: example currentActivated label ].
	example openWithSpec.
	^ example