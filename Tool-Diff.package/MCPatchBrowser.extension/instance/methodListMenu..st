*Tool-Diff
methodListMenu: aMenu

	selection 
		ifNotNil: [ 
			aMenu addList: #(
				('Install'	 installSelection)
				('Diff'	diffSelection)
				('Revert'	 revertSelection)
				-)].
	super methodListMenu: aMenu.
	^ aMenu
