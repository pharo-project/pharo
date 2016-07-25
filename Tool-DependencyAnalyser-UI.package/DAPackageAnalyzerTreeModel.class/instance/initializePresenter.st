initialization
initializePresenter
	super initializePresenter.
	self actionOnButtons.
	tree
		whenSelectedItemChanged: [ :item |
			item
				ifNil: [ buttonBrowseCycles enabled: false ]
				ifNotNil: [ 
					buttonBrowseCycles enabled: true.
					tree menu: [ :aMenu | self loadMenuFor: item in: aMenu ].
					self tree selectedItem browseInNautilus ] ]