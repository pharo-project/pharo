events
grabSelfOrTopRenderer: evt

	| renderer |
	
	renderer := self topRendererOrSelf.

	evt hand grabMorph: renderer.
	
	(renderer isKindOf: SystemWindow) ifTrue: [ 
		renderer position: evt hand position + (renderer position - evt startPoint)
	]