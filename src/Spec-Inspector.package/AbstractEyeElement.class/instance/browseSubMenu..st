menu
browseSubMenu: aMenu
	
	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Full';
				action: [ self browseSelectedObject ];
				shortcut: PharoShortcuts current browseShortcut  ].
			
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Class';
				action: [ self browseSelectedObjectClass ] ].
			
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Browse Hierarchy';
				action: [ self browseSelectedObjectClassHierarchy ];
				shortcut: $h meta ] ]