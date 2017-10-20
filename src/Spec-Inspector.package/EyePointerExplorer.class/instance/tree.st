accessing
tree
	^ tree ifNil: [ 
		tree := self instantiate: TreePresenter.
		tree whenBuiltDo: [ self initializeShortcuts ].
		tree 
			whenSelectedItemChanged: [ :item | item ifNotNil: [ self text doItReceiver: item value ]];	
			displayBlock:  [ :anObject | self labelFor: anObject ];
			menu:          [ :menu |  self inspectionMenu: menu ];
			hasChildrenBlock: [ :o | true  ];
			childrenBlock: [ :anObject | self childrenFor: anObject ];
			yourself]