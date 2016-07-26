accessing editing state
hasFocus
	^ ( editingState ifNil: [^ false ] ) hasFocus