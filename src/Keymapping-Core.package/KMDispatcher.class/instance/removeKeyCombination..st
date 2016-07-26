building
removeKeyCombination: aShortcut
	| keymap removalTarget |

	removalTarget := self directKeymaps.
	
	keymap := self keymapForShortcut: aShortcut.
	keymap ifNil: [ 
		self targets do: [ :e | (e keymapForShortcut: aShortcut) 
			ifNotNil: [ :s | 
				removalTarget := e category.
				keymap := s ] ] ].
	keymap ifNil: [ ^ self ].
	
	removalTarget removeKeymapEntry: keymap