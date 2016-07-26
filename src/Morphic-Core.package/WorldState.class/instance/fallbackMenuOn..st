worldmenu building
fallbackMenuOn: menu
	"Build the menu that is put up if something is going wrong with the menubuilder"
	
	menu addTitle: 'Fallback menu'.
	menu commandKeyHandler: self.
	menu addLine.
	menu
		defaultTarget: Smalltalk tools;
		addList: Smalltalk tools menuItems.
	menu addLine.
	menu add: 'Save'
		target: self class
		selector: #saveSession.
	menu add: 'Save as...'
		target: self class
		selector: #saveAs.
	menu add: 'Save and quit' 
		target: self class
		selector: #saveAndQuit.
	menu add: 'Quit' 
		target: self class
		selector: #quitSession.