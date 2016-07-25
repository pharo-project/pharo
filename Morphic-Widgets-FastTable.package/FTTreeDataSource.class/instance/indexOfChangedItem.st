updating
indexOfChangedItem
	"I should be call when the user of the Fast Tree collapse or extand an Item. I return the index of the changed item. I should not be used by an other method than #updateSelectionWithCollectBlock:."

	1 to: self numberOfRows do: [ :index | 
		(self elementAt: index) recentlyChanged
			ifTrue: [ ^ index ] ].
		
	^ self errorSubscriptBounds: self numberOfRows + 1