initialization
initialize

	unselectedItemsSearchingString := String new.
	selectedItemsSearchingString := String new.
	selectedSelectionList := Dictionary new.
	unselectedSelectionList := Dictionary new.
	
	super initialize.
	
	self vResizing: #shrinkWrap.
	self hResizing: #shrinkWrap.