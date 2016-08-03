populating menu
populateMenu: aMenu
	aMenu
		addGroup: [ :aGroup | 
			self menuActions
				do: [ :oldStyleMenuItemArray | 
					aGroup
						addItem: [ :anItem | 
							anItem
								name: oldStyleMenuItemArray first;
								description: oldStyleMenuItemArray third;
								icon: (Smalltalk ui icons iconNamed: oldStyleMenuItemArray fourth);
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
						icon: (Smalltalk ui icons iconNamed: #smallFindIcon);
						subMenu: self filtersSubMenu ] ].
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'File Out (ombu format)';
						description: 'Write selected entries to an Ombu file';
						icon: (Smalltalk ui icons iconNamed: #smallSaveAsIcon);
						action: [ self fileOutToOmbuSelection ] ].
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'File Out (cs format)';
						description: 'Write selected entries to a ChangeSet file';
						icon: (Smalltalk ui icons iconNamed: #smallSaveAsIcon);
						action: [ self fileOutToChangeSetSelection ] ] ].
	aMenu
		addGroup: [ :aGroup | 
			aGroup
				addItem: [ :anItem | 
					anItem
						name: 'Inspect';
						description: 'Open an inspector for each selected entry';
						icon: (Smalltalk ui icons iconNamed: #smallDebugIcon);
						shortcut: $i command;
						action: [ self inspectSelection ] ] ]