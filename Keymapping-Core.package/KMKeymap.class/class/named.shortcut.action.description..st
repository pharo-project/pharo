instance creation
named: keymapName shortcut: aShortcut action: anAction description: aDescription
	^ (self new)
		name: keymapName;
		defaultShortcut: aShortcut;
		shortcut: aShortcut;
		action: anAction;
		description: aDescription;
		yourself