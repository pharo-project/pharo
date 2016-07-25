selection
selection 
	self selectionIndex = 0 ifTrue: [ ^nil ].
	list ifNotNil: [ ^list at: self selectionIndex ].
	^ self getListItem: self selectionIndex