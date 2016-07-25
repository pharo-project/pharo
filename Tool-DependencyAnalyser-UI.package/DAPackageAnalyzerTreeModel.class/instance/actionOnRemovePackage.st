protocol
actionOnRemovePackage
	| packagesToRemove |
	self buttonRemovePackage
		action: [ 
			self selectedItemsFromTree
				ifNotEmpty: [ 
					packagesToRemove := self selectedItemsFromTree.
					packagesToRemove do: [ :item | self relationGraph removePackage: item content packageName ].
					self buildRoots ] ]