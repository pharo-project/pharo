private
forceChangesListRefresh
	| sel |
	
	sel := self selectedChangeSet.
	changesListPresenter items: self model allChanges.
	changesListPresenter setSelectedItem: sel.