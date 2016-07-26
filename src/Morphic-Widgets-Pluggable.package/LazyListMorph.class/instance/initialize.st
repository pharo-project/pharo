initialization
initialize
	super initialize.
	self color: self theme textColor.
	font := StandardFonts listFont.
	listItems := #().
	selectedRow := nil.
	selectedRows := PluggableSet integerSet.
	self adjustHeight.