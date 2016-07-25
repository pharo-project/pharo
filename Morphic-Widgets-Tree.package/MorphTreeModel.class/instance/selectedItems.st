accessing
selectedItems
	^ selection 
		ifNil: [OrderedCollection new]
		ifNotNil: [selection selectedItems]