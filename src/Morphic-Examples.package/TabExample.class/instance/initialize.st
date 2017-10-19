initialization
initialize

	super initialize.

	manager := TabManagerMorph new.
	manager
		when: #tabManagerDeleted send: #delete to: self.
	listModel := ListPresenter new.
	listModel items: ((1 to: 1000) asOrderedCollection)