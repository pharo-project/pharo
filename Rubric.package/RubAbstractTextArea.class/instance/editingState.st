accessing editing state
editingState
	^ editingState ifNil: [editingState := self newEditingState]