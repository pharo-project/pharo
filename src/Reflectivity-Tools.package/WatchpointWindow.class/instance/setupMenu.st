initialization
setupMenu
	menu := MenuPresenter new.
	menu applyTo: self.
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show More Items';
						icon: (self iconNamed: #up);
						action: [ self moreItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Show Less Items';
						icon: (self iconNamed: #down);
						action: [ self lessItems ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove All Items';
						icon: (self iconNamed: #smallDelete);
						action: [ self watchpoint deleteHistory ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Stop Monitoring';
						icon: (self iconNamed: #stop);
						action: [ self watchpoint stop ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Start Monitoring';
						icon: (self iconNamed: #smallPrintIt);
						action: [ self watchpoint start ] ] ].
	menu
		addGroup: [ :group | 
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Inspect ...';
						icon: (self iconNamed: #smallInspectIt);
						action: [ self watchpoint inspect ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Browse ...';
						icon: (self iconNamed: #nautilus);
						action:
								[ Smalltalk tools browser openOnMethod: self watchpoint compiledMethod ] ].
			group
				addItem: [ :item | 
					item
						name: nil;
						description: 'Remove !';
						icon: (self iconNamed: #delete);
						action: [ self watchpoint uninstall ] ] ]