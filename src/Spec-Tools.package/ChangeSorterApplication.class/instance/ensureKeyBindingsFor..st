shortcuts
ensureKeyBindingsFor: aWidget

	super ensureKeyBindingsFor: aWidget.
	
	self registerChangeSetShortcuts: changesListPresenter.
	self registerClassShortcuts: classesListPresenter.
	self registerMethodShortcuts: methodsListPresenter.