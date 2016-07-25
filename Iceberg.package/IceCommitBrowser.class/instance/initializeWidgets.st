initialization
initializeWidgets
	changes := self instantiate: IceChangesTreeView.
	diff := (self instantiate: DiffModel)
		showOptions: false;
		yourself.

	commit := self instantiate: IceCommitModel.
	
	self focusOrder
		add: changes;
		add: commit
