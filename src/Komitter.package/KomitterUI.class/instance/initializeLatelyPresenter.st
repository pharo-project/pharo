initialization
initializeLatelyPresenter
	tree
		whenHighlightedItemChanged:
			[ :node | 
			| item |
			item := node ifNil: [ nil ] ifNotNil: [ node content ].
			self refreshDiffForItem: item ].
	tree whenSelectedItemsChanged: [ self updateRemotes ]