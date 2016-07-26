accessing
font: aFont

	font := aFont.
	
	aFont ifNil: [ self updateFont ].
	
	familyName := font familyName.
	fontSize := font pointSize.