user interface
newScrolledTextAutoAccept
	| st |
	st := (RubTextFieldMorph
		on: self)
		withAdornment;
		withGhostText: 'Input field bla bla bla bla bla bla';
		autoAccept: true;
		yourself.
	st width: 300.
	^st