meta-actions
buildMetaMenu: evt
	| menu |
	menu := UIManager default newMenuIn: self for: self.
	self addStandardHaloMenuItemsTo: menu hand: evt hand.
	^ menu
