initialization
initialize
	super initialize.
	finder := nil.
	forceSearch := nil.
	searchingTextArea := nil.
	sourceTextArea := nil.
	useRegExCheckbox := nil.
	wrapBlockOrSelector := [ :i | i asString ] 