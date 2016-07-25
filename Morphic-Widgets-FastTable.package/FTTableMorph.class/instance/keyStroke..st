event handling
keyStroke: event
	self flag: #todo.	"If the function is explicit this should be redirect to the function widget."
	((super keyStroke: event) or: [ self navigationKey: event ])
		ifTrue: [ ^ true ].
		
	^ self keyStrokeSearch: event