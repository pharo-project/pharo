editing
delete
	selection isEmpty ifTrue: [ 
		selection end moveRight.
	 ].

	selection isEmpty ifFalse: [ 
		selection cut.
		self announce: (TxDeletedTextAnnouncement new editor: self; yourself) ].

	