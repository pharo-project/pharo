accessing
keyStroke: anEvent

	anEvent keyString =  '<Cmd-a>'
		ifTrue: [ self selectAll ].
		
	anEvent keyString =  '<Cmd-A>'
		ifTrue: [ self deselectAll ]