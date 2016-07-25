as yet unclassified
selectedItem
	"Answer the selected list item or nil if cancelled."

	^self cancelled ifFalse: [self listMorph selectedItem]