populating menu
filtersSubMenu
	
	| aMenu |
	aMenu := MenuModel new.
	
	aMenu addGroup: [ :aGroup |
		
		aGroup addItem: [ :anItem |
			anItem 
				name: 'all changes to this method';
				description: 'Filter any addition, modification or removal of this method.';
				shortcut: $f command, $m command;
				action: [ self filterAllChangesToSelection ] ].
				
		aGroup addItem: [ :anItem |
			anItem 
				name: 'all changes to this package';
				description: 'Filter any addition, modification or removal of this package.';
				shortcut: $f command, $p command;
				action: [ self filterAllChangesToSelectionPackage ]
			].

		EpPluggableFilter allFilters do: [ :aFilter |
			aGroup addItem: [ :anItem |
				anItem 
					name: aFilter title;
					description: aFilter title;
					action: [ self filterWith: aFilter ] ] ]
		].
	
	^ aMenu.