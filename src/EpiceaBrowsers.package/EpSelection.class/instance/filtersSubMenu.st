populating menu
filtersSubMenu
	
	| aMenu |
	aMenu := MenuModel new.
	
	aMenu addGroup: [ :aGroup |

		aGroup addItem: [ :anItem |
			anItem 
				name: 'remove all filters';
				icon: (self iconNamed: #smallDelete);
				action: [ self removeAllFilters ] ].

		aGroup addItem: [ :anItem |
			anItem 
				name: 'done today';
				icon: (self iconNamed: #calendar);
				action: [ self filterToday ] ].

		aGroup addItem: [ :anItem |
			anItem 
				name: 'done earlier';
				icon: (self iconNamed: #calendar);
				action: [ self filterBefore ] ].

		aGroup addItem: [ :anItem |
			anItem 
				name: 'done afterwards';
				icon: (self iconNamed: #calendar);
				action: [ self filterAfter ] ].

		
		aGroup addItem: [ :anItem |
			anItem 
				name: 'is change to this method';
				description: 'Show only additions, modifications or removals of this method.';
				icon: (self iconNamed: #smallFind);
				shortcut: $f command, $m command;
				action: [ self filterAllChangesToSelection ] ].
				
		aGroup addItem: [ :anItem |
			anItem 
				name: 'is change to this package';
				description: 'Show only additions, modifications or removals of this package.';
				icon: (self iconNamed: #smallFind);
				shortcut: $f command, $p command;
				action: [ self filterAllChangesToSelectionPackage ]
			].

		EpPluggableFilter allFilters do: [ :aFilter |
			aGroup addItem: [ :anItem |
				anItem 
					name: aFilter title;
					icon: (self iconNamed: #smallFind);
					description: aFilter title;
					action: [ self filterWith: aFilter ] ] ] ].
	
	^ aMenu