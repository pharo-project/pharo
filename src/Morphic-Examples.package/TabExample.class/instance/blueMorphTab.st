tabs creation
blueMorphTab
	| tab |
	tab := Tab
		label: 'Blue Morph'
		icon: (self iconNamed: #smallBackIcon)
		morph: Morph new.
	tab
		actions:
			{(TabAction
				action: [ tab morph color: Color random ]
				icon: (self iconNamed: #smallConfigurationIcon)
				label: 'Randomize color')}.
	tab
		closeable: false;
		menu: [ :menu | menu add: 'Fubu' target: self selector: #halt ].
	^ tab