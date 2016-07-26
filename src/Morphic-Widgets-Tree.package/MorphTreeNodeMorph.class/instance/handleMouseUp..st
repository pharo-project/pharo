mouse events
handleMouseUp: anEvent 
	container enabled ifFalse: [ ^ false ].
	(container commandOrCrontrolKeyPressed: anEvent) 
		ifTrue: [^ container listManager mouseUp: anEvent on: self].
	^ super handleMouseUp: anEvent