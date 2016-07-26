brick-layout-events
onChildrenLayouted
	super onChildrenLayouted.
	
	self band translateXTo: (self band data ifNil: [ 0 ]).
	self scrollToTheLastOne