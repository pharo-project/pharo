api
updateChangesList

	| sel |
	sel := self selectedChangeSet.
	changesListModel items: self model allChanges.
	changesListModel setSelectedItem: sel.