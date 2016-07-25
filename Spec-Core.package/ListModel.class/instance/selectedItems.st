api
selectedItems
	"<api: #inspect>"
	"Return all the selected items in the case of a multiple selection list"

	^ multiSelectionHolder keys select: [:index | multiSelectionHolder at: index ] thenCollect: [:index | self listElementAt: index ]