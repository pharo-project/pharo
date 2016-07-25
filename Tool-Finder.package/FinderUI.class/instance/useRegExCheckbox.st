items creation
useRegExCheckbox
	^ useRegExCheckbox ifNil: [self buildRegExChooser]
