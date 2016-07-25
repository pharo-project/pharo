menu
inspectionSubMenu: aMenu

	aMenu addGroup: [:aGroup |
		aGroup addItem: [ :item |
			item
				name: 'Inspect' translated;
				action: [ self inspectSelectedObjectInNewWindow ];
				shortcut: $i meta ].
		aGroup addItem: [ :item |
			item
				name: 'Explore' translated;
				action: [ self exploreSelectedObject ];
				shortcut: $i meta shift ].

	self customMenuActions cull: aMenu cull: aGroup.
			
	]