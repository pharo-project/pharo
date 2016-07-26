protocol
actionOnAddPackage
	| itemsToAdd itemsChosen itemsAdded |
	buttonAddPackage
		action: [ 
			self selectedItemsFromPackageList
				ifNotEmpty: [ 
					itemsToAdd := self selectedItemsFromPackageList asOrderedCollection.
					itemsAdded := self itemsFromPackageChosenList asOrderedCollection.
					itemsChosen := self merge: itemsToAdd With: itemsAdded.
					self packageChosenList items: itemsChosen asSortedCollection.
					self updatePackageChosenLabel ] ]