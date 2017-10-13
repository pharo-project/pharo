accessing
tree
	^ tree
		ifNil: [ tree := self instantiate: TreePresenter.
			tree whenBuiltDo: [ self initializeShortcuts ].
			tree
				whenSelectedItemChanged: [ :item | item ifNotNil: [ self elementChanged ] ];
				childrenBlock: [ :anObject | self childrenFor: anObject ];
				iconBlock: [ :treeNode | self iconFor: treeNode ];
				displayBlock: [ :anObject | self labelFor: anObject ];
				menu: [ :menu | self inspectionMenu: menu ];
				doubleClick: [ self diveInto: self selectedObject ];
				yourself ]