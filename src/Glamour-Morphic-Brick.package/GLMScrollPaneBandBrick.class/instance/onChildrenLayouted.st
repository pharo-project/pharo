brick-layout-events
onChildrenLayouted
	super onChildrenLayouted.
	
	self scrollpane ifNotNil: [ :s | s updateScrollbar ].
	
	self shouldLayoutMorphs ifFalse: [ ^ self ].
	self asyncTask
		perform: [ self updateMorphBounds ]
		delay: 400
		priority: Processor userBackgroundPriority + 1