morphic ui
getMenu: aMenu
	index < 2 ifTrue: [^ aMenu].
	self fillMenu: aMenu fromSpecs: 
		(Array
			with: (Array with: 'view changes -> ', ancestry name with: #viewChanges)
			with: #('spawn history' spawnHistory)).
	^ aMenu