updating
updateDefinedClassNamed: oldString withNewName: newString
	(self hasClassNamed: oldString)
		ifFalse: [ ^ self ].
	self removeClassNamed: oldString.
	self addClassNamed: newString