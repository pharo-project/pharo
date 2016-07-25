actions
installMenu

	| menu |
	menu := UIManager default newMenuIn: self for: self.
	menu add: 'load packages' selector: #load.
	menu add: 'merge packages' selector: #merge.
	menu add: 'upgrade packages' selector: #upgrade.
	menu popUpInWorld.