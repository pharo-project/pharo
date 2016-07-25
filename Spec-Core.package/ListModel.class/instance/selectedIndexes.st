api
selectedIndexes
	"<api: #inspect>"
	"Return the indexes of selected items on the case of a multiple selection list"
	
	^ multiSelectionHolder keys select: [:index | multiSelectionHolder at: index ]