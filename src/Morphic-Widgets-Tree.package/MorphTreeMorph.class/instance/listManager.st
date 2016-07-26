accessing
listManager
	^ listManager ifNil: [listManager := MorphTreeListManager new client: self] 