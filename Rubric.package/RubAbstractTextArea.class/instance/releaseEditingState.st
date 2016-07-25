accessing editing state
releaseEditingState
	editingState ifNotNil: [ 
		editingState unplug.
		editingState := nil]