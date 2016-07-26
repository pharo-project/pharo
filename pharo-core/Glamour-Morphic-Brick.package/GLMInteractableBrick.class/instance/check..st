brick-interactable-actions
check: aBoolean
	aBoolean
		ifTrue: [ self check ]
		ifFalse: [ self uncheck ]