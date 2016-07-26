keymap building
named: keymapName do: anActionBlock withDescription: aDescription
	^ KMKeymap
		named: keymapName
		shortcut: self
		action: anActionBlock
		description: aDescription