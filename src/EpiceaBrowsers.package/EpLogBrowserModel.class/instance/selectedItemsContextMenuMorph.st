menu
selectedItemsContextMenuMorph

	| aMenu |
	aMenu := MenuModel new.
	aMenu
		addGroup: [ :aGroup | 
			self menuActions
				do: [ :oldStyleMenuItemArray | 
					aGroup
						addItem: [ :anItem | 
							anItem
								name: oldStyleMenuItemArray first;
								description: oldStyleMenuItemArray third;
								icon: (self iconNamed: oldStyleMenuItemArray fourth);
								shortcut: oldStyleMenuItemArray fifth;
								action: [ self perform: oldStyleMenuItemArray second ] ] ] ].

"	Useless feature well until drag&drop is fixed. See case 18612.
	
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Open in Sorter';
						description: 'Open a new Sorter with the selected entries';
						icon: EpSorterView icon;
						shortcut: $s command;
						action: [ self spawnSorterWithSelection ] ] ]."
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Filter';
						icon: (self iconNamed: #smallFindIcon);
						subMenu: self filtersSubMenu ] ].
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'File Out';
						description: 'Write selected entries to an Ombu file';
						icon: (self iconNamed: #smallSaveAsIcon);
						action: [ self fileOutSelection ] ] ].
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Inspect';
						description: 'Open an inspector for each selected entry';
						icon: (self iconNamed: #smallDebugIcon);
						shortcut: $i command;
						action: [ self inspectSelection ] ] ].
				
	^ aMenu buildWithSpecAsPopup