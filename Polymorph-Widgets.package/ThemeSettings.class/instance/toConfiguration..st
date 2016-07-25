settings
toConfiguration: tree
	"Update the tree from the receiver."
	
	tree at: #windowColor put: self windowColor.
	tree at: #autoSelectionColor put: self autoSelectionColor.
	tree at: #selectionColor put: self basicSelectionColor