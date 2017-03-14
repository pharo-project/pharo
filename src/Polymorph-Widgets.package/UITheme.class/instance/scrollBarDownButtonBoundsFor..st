morph creation
scrollBarDownButtonBoundsFor: aScrollbar
	
	| extent position |
	
	extent := aScrollbar buttonExtent.
	position := aScrollbar innerBounds bottomRight - extent.	

	^ position extent: extent