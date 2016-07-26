menu - change set
rename

	| set |
	set := self model rename: self selectedChangeSet.
	
	changesListModel 
		updateList;
		setSelectedItem: set