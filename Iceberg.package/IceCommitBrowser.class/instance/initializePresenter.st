initialization
initializePresenter
	changes whenHighlightedItemChanged: [ :node | self updateDiff: node ].
