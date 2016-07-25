button behavior
removeSelectedPackageFromSelectedGroup
	| packageToRemove selection |
	selection := self groupTreeModel selection selectedNodePath.
	selection ifNil: [ ^ self ].
	packageToRemove := selection last. 
	packageToRemove isGroup
		ifFalse: [ 
			self selectedGroup
				ifNotNil: [ :group | 
					self groupManager remove: {packageToRemove item} from: group.
					groupTree updateList.
					groupTreeModel hardlySelectItem: group ] ]