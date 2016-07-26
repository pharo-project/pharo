initialization
initializePresenter

	remotes whenHighlightedItemChanged: [ :item | 
		editRemote enabled: item notNil.
		removeRemote enabled: item notNil ].
