initialization
initialize
	super initialize.
	self initializeValueHolders.	
	self registerEvents.

	self
		bindKeyCombination: Character space
		toAction: [ self clickOnSelectedItem ]