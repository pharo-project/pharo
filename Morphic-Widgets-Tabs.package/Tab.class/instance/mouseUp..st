event handling
mouseUp: event

	(event commandKeyPressed or: [ event controlKeyPressed ])
		ifTrue: [ self toggleSelection ]
		ifFalse: [ 
			event shiftPressed
				ifTrue: [ self selectToMe ]
				ifFalse: [ self selected: true ] ]