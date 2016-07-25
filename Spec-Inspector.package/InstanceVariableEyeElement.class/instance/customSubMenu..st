menu
customSubMenu: aMenu
	
	aMenu addGroup: [ :aGroup | 
		aGroup 
			addItem: [:anItem | 
				anItem 
					name: 'List Methods Using "', self instVarName, '"';
					action: [ self referencesToInstanceVariable] ];
			addItem: [:anItem | 
				anItem 
					name: 'List Methods Storing into "', self instVarName, '"';
					action: [ self storingsIntoInstanceVariable].
			].
		]