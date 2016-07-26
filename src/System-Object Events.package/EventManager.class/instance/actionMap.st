accessing
actionMap
	^ actionMap
		ifNil:  [ self createActionMap ]
		ifNotNil: [ actionMap ]