morphic ui
repositoryListMenu: aMenu
	self repository ifNil: [^ aMenu].
	self fillMenu: aMenu fromSpecs: self repositoryMenuSpec.
	^ aMenu
				