unselectedList
unselectedSelectionAt: index put: aBoolean
	"Mark the item as selected or not."
	
	unselectedSelectionList at: index put: aBoolean.
	self changed: #hasUnselectedSelections