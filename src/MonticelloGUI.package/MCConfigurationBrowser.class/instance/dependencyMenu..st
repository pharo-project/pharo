morphic ui
dependencyMenu: aMenu
	self fillMenu: aMenu fromSpecs: #(('Change log...' showChangeLog)).
	self fillMenu: aMenu fromSpecs: #(('Add dependency...' addDependency)).
	self selectedDependency ifNotNil: [
		self fillMenu: aMenu fromSpecs: #(('Remove dependency...' remove))].
	^aMenu
