user interface
newScrolledText
	| st |
	st := (RubTextFieldMorph on: self)
		withAdornment;
		withGhostText: 'Input field bla bla bla bla bla bla';
		yourself.
	st width: 300.
	st textArea forbidMenu.
	^ st