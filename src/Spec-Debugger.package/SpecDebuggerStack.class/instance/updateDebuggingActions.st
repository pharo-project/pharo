updating
updateDebuggingActions

	actionsCache ifNotNil: [ 
		actionsCache do: [ :each |
			each keymap ifNotNil: [ :aShortcut |
				self list removeKeyCombination: aShortcut ] ] ].
	actionsCache := nil.
	self generateContextMenuDebuggingActions.
	
	actionsCache do: [ :each |
		each keymap ifNotNil: [ 
			self list bindKeyCombination: each keymap toAction:  [ each execute ] ] ].