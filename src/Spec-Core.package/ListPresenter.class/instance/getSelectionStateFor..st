private
getSelectionStateFor: anIndex
	"Return the current state of the item -if selected or not - in a multiple selection list"
	"Answer true if the item at index _anIndex_ is selected"
	
	^ (multiSelectionHolder at: anIndex ifAbsent: [ ^ false ]) == true