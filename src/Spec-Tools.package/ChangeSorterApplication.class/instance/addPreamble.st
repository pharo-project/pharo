menu - change set
addPreamble

	self model addPreambleTo: self selectedChangeSet.
	changesListPresenter setSelectedItem: self selectedChangeSet