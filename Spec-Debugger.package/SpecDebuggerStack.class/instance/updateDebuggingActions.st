updating
updateDebuggingActions

	actionsCache ifNotNil: [ 
		actionsCache do: [ :each |
			each shortcutCommand ifNotNil: [ :aShortcut |
				self list removeKeyCombination: aShortcut ] ] ].
	actionsCache := nil.
	self generateContextMenuDebuggingActions.
	
	actionsCache do: [ :each |
		each shortcutCommand ifNotNil: [ 
			self list bindKeyCombination: each shortcutCommand toAction:  [ each execute ] ] ].