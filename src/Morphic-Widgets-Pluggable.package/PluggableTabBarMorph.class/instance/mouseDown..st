actions
mouseDown: anEvent
	| xPosition newTab |
	xPosition := anEvent cursorPoint x.
	newTab :=
		((self tabs detect: [ :anAssociation | | tabBounds |
				tabBounds := anAssociation key bounds.
				xPosition between: tabBounds left and: tabBounds right]
			ifNone: [nil])
		key).
	newTab ifNil: [^ self].
	newTab = activeTab ifFalse: [ self activeTab: newTab ]
