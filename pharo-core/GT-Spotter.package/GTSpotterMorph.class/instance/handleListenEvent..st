events-processing
handleListenEvent: anEvent
		
	anEvent type = #mouseDown ifTrue: [ 
		((self fullBoundsInWorld containsPoint: anEvent position) not and: [ self autoHide ])
			ifTrue: [ self spotterModel ifNotNil: [ :aModel | aModel exitDueTo: #mouseDown ] ] ].
		
	anEvent type = #keyDown ifTrue: [ 
		self spotterModel
			ifNotNil: [ :s | s shiftPressed: (InputEventSensor default anyModifierKeyPressed not & InputEventSensor default shiftPressed) ] ].