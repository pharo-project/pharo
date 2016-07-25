initialization
setupMenu
	menu := MenuModel new.
	menu applyTo: self.
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show More Items';
						icon: (Smalltalk ui icons iconNamed: #up);
						action: [ self moreItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show Less Items';
						icon: (Smalltalk ui icons iconNamed: #down);
						action: [ self lessItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove All Items';
						icon: (Smalltalk ui icons iconNamed: #smallDelete);
						action: [ self watchpoint deleteHistory ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Stop Monitoring';
						icon: (Smalltalk ui icons iconNamed: #stop);
						action: [ self watchpoint stop ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Start Monitoring';
						icon: (Smalltalk ui icons iconNamed: #smallPrintIt);
						action: [ self watchpoint start ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Inspect ...';
						icon: (Smalltalk ui icons iconNamed: #smallInspectIt);
						action: [ self watchpoint inspect ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Browse ...';
						icon: (Smalltalk ui icons iconNamed: #nautilus);
						action:
								[ Smalltalk tools browser openOnMethod: self watchpoint compiledMethod ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove !';
						icon: (Smalltalk ui icons iconNamed: #delete);
						action: [ self watchpoint uninstall ] ] ]