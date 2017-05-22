actions
confirmSelection
	acceptBlock ifNotNil: [ 
		acceptBlock value: selectedItems ].
	self window delete