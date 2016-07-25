selectedList
selectedSelectionAt: index put: aBoolean
	"Mark the item as selected or not."
	
	selectedSelectionList at: index put: aBoolean.
	self changed: #hasSelectedSelections