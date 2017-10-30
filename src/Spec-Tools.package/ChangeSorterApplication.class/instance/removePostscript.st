menu - change set
removePostscript

	self model removePostscriptFrom: self selectedChangeSet.
	changesListPresenter setSelectedItem: self selectedChangeSet