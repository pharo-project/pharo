accessing
selectionPathItems
	^ self selectedNodePath isNil ifTrue: [nil] ifFalse: [self selectedNodePath collect: [:each | each item]]