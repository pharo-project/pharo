checkbox-actions
toggle

	self isChecked
		ifTrue: [ self uncheck ]
		ifFalse: [ self check ]