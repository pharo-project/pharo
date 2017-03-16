initialization
initializeFiltersModel

	filtersModel
		iconBlock: [ self iconNamed: #smallFind ];
		displayBlock: [ :aFilter | aFilter title ];
		menu: [ :aMenu | 
			aMenu addGroup: [ :aGroup | 
				aGroup addItem: [ :anItem | 
					anItem
						name: 'Remove';
						description: 'Remove this filter';
						icon: (self iconNamed: #smallDeleteIcon);
						shortcut: $x command;
						action: [ self removeFilter: filtersModel selectedItem content ] ] ] ]