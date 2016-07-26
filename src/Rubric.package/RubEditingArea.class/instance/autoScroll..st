event handling
autoScroll: anEvent
	scrollPane ifNil: [ ^self ].
	self scrollPivot  ifNil: [ ^self ].
	scrollPane scrollWhenMouseMove: anEvent pivotPoint: self scrollPivot