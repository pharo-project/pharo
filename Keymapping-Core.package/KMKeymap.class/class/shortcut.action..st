instance creation
shortcut: aShortcut action: anAction
	^ (self new)
		defaultShortcut: aShortcut;
		shortcut: aShortcut;
		action: anAction;
		yourself