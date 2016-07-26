protocol
selectedItem
	"Answer the selected list item."

	^(self listSelectionIndex between: 1 and: self list size) ifTrue: [
		self list at: self listSelectionIndex]