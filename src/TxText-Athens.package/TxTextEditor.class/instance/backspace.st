editing
backspace
	selection isEmpty ifTrue: [ 
		selection end moveLeft.
	 ].

	selection isEmpty ifFalse: [ 
		selection cut.
		self notifyDeleted ].