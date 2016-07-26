initialization
initializePresenter
	super initializePresenter.
	
	buttonReversed action: [ self reversedRoots  ].
	
	
	
	tree
		whenSelectedItemChanged: [ :item | 
			item
				ifNotNil: [ 
					self tree selectedItem browseInNautilus.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ] ] ]