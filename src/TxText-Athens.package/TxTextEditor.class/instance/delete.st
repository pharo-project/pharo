editing
delete
	selection isEmpty ifTrue: [ 
		selection end moveRight.
	 ].

	selection isEmpty ifFalse: [ 
		selection cut.
		self notifyDeleted ].

	