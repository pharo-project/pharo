accessing
toolbarBox: anObject

	toolbarBox := anObject.
	self setLabelWidgetAllowance.
	self replacePane: menuBox with: toolbarBox