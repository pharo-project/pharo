event handling
checkOrUncheck

	(self stepModel isSelected: self candidateLink)
		ifTrue: [ self isChecked ifFalse: [ self check ] ]
		ifFalse: [ self isChecked ifTrue: [ self uncheck ] ]