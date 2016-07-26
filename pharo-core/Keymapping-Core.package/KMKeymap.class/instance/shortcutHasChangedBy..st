accessing
shortcutHasChangedBy: aShortcut
	self defaultShortcut = self shortcut ifTrue: [ self shortcut: aShortcut ].
	self defaultShortcut: aShortcut