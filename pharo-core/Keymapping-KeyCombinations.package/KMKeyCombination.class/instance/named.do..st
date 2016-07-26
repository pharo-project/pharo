keymap building
named: keymapName do: anActionBlock
	^ KMKeymap
		named: keymapName
		shortcut: self
		action: anActionBlock