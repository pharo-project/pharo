button behavior
addSelectedPackageInSelectedGroup
	| selection |
	selection := self groupTreeModel selection selectedNodePath.
	selection ifNil: [ ^ self ].
	selection last isGroup 
		ifTrue: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					self groupManager add: self selectedElements into: group.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]