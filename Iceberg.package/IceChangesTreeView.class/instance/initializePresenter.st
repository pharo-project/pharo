initialization
initializePresenter
	changeSetHolder whenChangedDo: [ :changeSet | 
		self initializeRoots.
		changeSet whenChangedDo: [ 
			tree highlightedItem: nil.
			self initializeRoots. ]
 	]
