initialization
initialize
	super initialize.
	self rewriteRule
		replace: 'UITheme current'
			with: 'Smalltalk ui theme';
		replace: 'ThemeIcons current'
			with: 'Smalltalk ui icons'