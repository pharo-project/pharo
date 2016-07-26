menus
setListMenu
	listModel menu: [ :menu |
		menu addAllFromPragma: 'messageBrowserListMenu' target: self ].