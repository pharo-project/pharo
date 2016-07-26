updating
updateSizing
	| old |
	old := sizing.
	self basicUpdateSizing.
	sizing = old ifFalse: [self layoutPanes].
	^sizing