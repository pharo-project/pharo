instance creation
named: keymapName shortcut: aShortcut action: anAction
	^ (self new)
		name: keymapName;
		defaultShortcut: aShortcut;
		shortcut: aShortcut;
		action: anAction;
		yourself