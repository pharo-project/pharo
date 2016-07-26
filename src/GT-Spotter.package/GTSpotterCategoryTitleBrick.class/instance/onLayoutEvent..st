layouting
onLayoutEvent: aLayoutEvent
	super onLayoutEvent: aLayoutEvent.
	
	(aLayoutEvent isKindOf: GLMBrickLayoutStacked) ifTrue: [ "self enable" ]