dropping/grabbing
justDroppedInto: aMorph event: evt
	| halo |
	super justDroppedInto: aMorph event: evt.
	halo := evt hand halo.
	(halo notNil and:[halo target hasOwner: self]) ifTrue:[
		"Grabbed single menu item"
		self addHalo: evt.
	].
	stayUp ifFalse:[evt hand newMouseFocus: self].