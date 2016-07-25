initialization
initializePresenter
	super initializePresenter.
	
	tree
		whenSelectedItemChanged: [ :item | 
			item
				ifNotNil: [ 
					self tree selectedItem browseInNautilus.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ] ] ].