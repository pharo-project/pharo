accessing
renameChangesForClass: oldClassName to: newClassName
	^ self changeClassName = oldClassName
		ifFalse: [ self ]
		ifTrue: [
			self copy
				changeClassName: newClassName;
				yourself ]