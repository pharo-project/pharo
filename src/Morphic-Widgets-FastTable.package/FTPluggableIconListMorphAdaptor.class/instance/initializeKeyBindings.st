initialization
initializeKeyBindings 
	super initializeKeyBindings.
	self 
		bindKeyCombination: Character space asKeyCombination
		toAction: [ self interactWithSelection ].
	