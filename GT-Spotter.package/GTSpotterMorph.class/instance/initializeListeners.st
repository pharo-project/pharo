initialization
initializeListeners
	self activeHand addEventListener: self.
	Morph announcer weak when: MorphLostFocus send: #onLostFocus: to: self.
	self bindingDictionnary
		associationsDo: [ :anAssociation | self bindKeyCombination: anAssociation key toAction: anAssociation value ].
	processorKeyBindings := Dictionary new.