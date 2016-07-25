accessing
selectedObjectDo: aBlock
	self selectedElement ifNotNil: [ :element |
		aBlock value: element value ]