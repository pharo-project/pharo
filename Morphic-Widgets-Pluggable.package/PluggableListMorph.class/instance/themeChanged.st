updating
themeChanged

	self setProperty: #textColor toValue: self theme textColor; updateList.
	
	self selectionColor ifNotNil: [
		self selectionColor: self theme selectionColor
	].

	super themeChanged 