api
selectedItemsSorted
	
	"return all the selected items sorted by their index"

	^ (multiSelectionHolder keys 
			select: [:index | multiSelectionHolder at: index ]) sort 
				collect: [:index | self listElementAt: index ]