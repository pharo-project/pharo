example
example
	| example |
	example := RadioButtonGroupModel new.
	example
		addRadioButton: (RadioButtonModel new label: '1');
		addRadioButton: (RadioButtonModel new label: '2');
		addRadioButton: (RadioButtonModel new label: '3');
		whenCurrentActivatedChanged: [ self inform: example currentActivated label ].
	example openWithSpec.
	^ example