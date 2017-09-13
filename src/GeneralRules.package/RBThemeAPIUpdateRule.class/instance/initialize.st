initialization
initialize
	super initialize.
	self
		replace: 'UITheme current'
			with: 'Smalltalk ui theme';
		replace: 'ThemeIcons current'
			with: 'Smalltalk ui icons'