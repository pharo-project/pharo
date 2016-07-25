morphic ui
versionListMenu: aMenu
	1 to: self orderSpecs size do: [ :index |
		aMenu addUpdating: #orderString: target: self selector: #order: argumentList: { index } ].
	^aMenu