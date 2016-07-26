volume menu
volumeMenu: aMenu
	aMenu
		add: 'Delete directory...' translated
		selector: #deleteDirectory.
	aMenu addLine.
	aMenu
		addServices: (self itemsForDirectory: self directory)
		for: self
		extraLines: #().
	^aMenu.