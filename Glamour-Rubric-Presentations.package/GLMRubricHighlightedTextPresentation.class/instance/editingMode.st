accessing
editingMode
	^ editingMode isNil 
		ifTrue: [nil] 
		ifFalse: [editingMode glamourValue: self]