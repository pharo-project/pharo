menu
endSubMenu: aMenu 
	
	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem 
				name: 'Copy Name';
				action: [ self copyAccessorCode ];
				shortcut: $c meta ] ]