accessing
selectedItemOrItemsOrNil
	^ self lastSelectedNode isNil 
		ifTrue: [ nil ]
		ifFalse: [ self lastSelectedNode item ]