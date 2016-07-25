buttons behavior
unselectedSelectionItems

	| result |
	result := OrderedCollection new.
	self unselectedItemsProbablyRestricted doWithIndex: [:item :index |
		(self unselectedSelectionAt: index)
			ifTrue: [result add: item]].
	^result