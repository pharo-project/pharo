actions
loadMenuFor: item in: aMenu
	| menuGroup selectedNodes |
	selectedNodes := self selectedItemsFromTree.
	menuGroup := MenuGroupPresenter new.
	self addPackageOptionsFor: selectedNodes on: menuGroup.
	self addDependenciesOptionsFor: selectedNodes asSet on: menuGroup.
	(selectedNodes size = 1)
		ifTrue: [ selectedNodes first addMenuEntriesOn: menuGroup ].
	
	aMenu addMenuGroup: menuGroup.
	^ aMenu