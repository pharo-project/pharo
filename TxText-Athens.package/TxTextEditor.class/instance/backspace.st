editing
backspace
	selection isEmpty ifTrue: [ 
		selection end moveLeft.
	 ].

	selection isEmpty ifFalse: [ 
		selection cut.
		self announce: (TxDeletedTextAnnouncement new editor: self; yourself) ].