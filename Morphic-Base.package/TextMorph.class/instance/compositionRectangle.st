private
compositionRectangle
	| compRect |
	compRect := self innerBounds.
	compRect := compRect insetBy: margins.
	compRect width < 9 ifTrue: [compRect := compRect withWidth: 9].
	compRect height < 16 ifTrue: [compRect := compRect withHeight: 16].
	^ compRect