accessing
editingState
	^ editingState ifNil: [editingState := self editingStateClass new]