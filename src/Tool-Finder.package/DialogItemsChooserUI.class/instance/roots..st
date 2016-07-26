private
roots: aTree
	aTree == unselectedItemsModel
		ifTrue: [ ^ self unselectedItemsProbablyRestricted].
	aTree == selectedItemsModel
		ifTrue:[ ^ self selectedItemsProbablyRestricted].