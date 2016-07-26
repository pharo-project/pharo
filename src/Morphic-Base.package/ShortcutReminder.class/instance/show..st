shortcut creation
show: aMenuItem

	| shortcutMorph |
	
	shortcutMorph := self createShortcutMorphFor: aMenuItem.
	
	self positionShortcut: shortcutMorph.
	
	shortcutMorph openInWorld.