menus
classListMenu: aMenu 
	classSelection ifNil: [ ^aMenu ].
	super classListMenu: aMenu.
	aMenu
		addLine;
		add: ('Load class {1}'  format: {classSelection})
		selector: #loadClassSelection.
	^ aMenu