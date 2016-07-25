protocol
actionOnRemovePackage
	| itemsAdded |
	buttonRemovePackage
		action: [ 
			self selectedItemsFromPackageChosenList
				ifNotEmpty: [ 
					itemsAdded := self itemsFromPackageChosenList.
					self selectedItemsFromPackageChosenList do: [ :each | itemsAdded remove: each ].
					self packageChosenList items: itemsAdded.
					self packageChosenList resetSelection.
					self updatePackageChosenLabel ] ]