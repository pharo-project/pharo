shortcuts
ensureKeyBindingsFor: aWidget

	super ensureKeyBindingsFor: aWidget.
	
	self registerChangeSetShortcuts: changesListModel.
	self registerClassShortcuts: classesListModel.
	self registerMethodShortcuts: methodsListModel.