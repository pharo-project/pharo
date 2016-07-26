menu
customSubMenu: aMenu
	"Subclasse may add more menu items here"

	aMenu addGroup: [ :aGroup |
		aGroup addItem: [ :anItem |
			anItem name: 'Inst var refs...';
			action: [ self browseInstVarRefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Inst var defs...';
			action: [ self browseInstVarDefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class var refs...';
			action: [ self classVarRefs ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class variables';
			action: [ self browseInstClassVariables ] ].
		aGroup addItem: [ :anItem |
			anItem name: 'Class refs';
			action: [ self browseClassRefs ] ] ]