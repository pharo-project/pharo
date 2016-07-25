initialize
initialize
	super initialize.
	self compose.
	self addCursor.
	self plugFindReplace.
	DefaultTextColor := self theme textColor