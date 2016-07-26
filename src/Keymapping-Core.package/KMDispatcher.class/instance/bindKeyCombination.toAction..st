building
bindKeyCombination: aShortcut toAction: anAction
	self directKeymaps 
		addKeymapEntry: (KMKeymap
			shortcut: aShortcut 
			action: anAction)