menu - change set
addPreamble

	self model addPreambleTo: self selectedChangeSet.
	changesListModel setSelectedItem: self selectedChangeSet