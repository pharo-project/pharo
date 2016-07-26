initialization
initializeFiltersModel
	filtersModel
		displayBlock: [ :aFilter | aFilter title ];
		menu: [ :aMenu | 
			aMenu
				addGroup: [ :aGroup | 
					aGroup
						addItem: [ :anItem | 
							anItem
								name: 'Remove';
								description: 'Open a new Sorter with the selected entries';
								icon: (Smalltalk ui icons iconNamed: #smallDeleteIcon);
								shortcut: $x command;
								action: [ self removeFilter: filtersModel selectedItem content.
									self refresh ] ] ] ]