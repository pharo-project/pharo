buttons behavior
selectedSelectionItems

	| result |
	result := OrderedCollection new.
	self selectedItemsProbablyRestricted doWithIndex: [:item :index |
		(self selectedSelectionAt: index)
			ifTrue: [result add: item]].
	^result