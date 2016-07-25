actions
addDependenciesOptionsFor: selectedNodes on: menuGroup
	| namesOfSelected |
	(self areClassToDependencyNodes: selectedNodes)
		ifTrue: [ 
			namesOfSelected := selectedNodes collect: [ :package | package content theClass ].
			menuGroup
				addMenuItem:
					(MenuItemModel new
						name: 'Browse dependencies from class';
						action: [ self openTreeFor: namesOfSelected ]) ]