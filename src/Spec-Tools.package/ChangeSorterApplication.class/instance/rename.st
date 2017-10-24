menu - change set
rename

	| set |
	set := self model rename: self selectedChangeSet.
	
	changesListPresenter 
		updateList;
		setSelectedItem: set