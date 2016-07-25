text area protocol
scrollWhenMouseMove: anEvent pivotPoint: aPivot
	| worldTransform viewBounds gap |
	worldTransform := self transformFrom: self world.
	(self scrollSelectionIntoView: anEvent)
		ifTrue: [ ^ self ].
	self textArea selectionRects
		ifNotEmpty: [ :selRects |
			viewBounds := worldTransform localBoundsToGlobal: self bounds.
			(viewBounds containsPoint: anEvent hand position)
				ifFalse: [ 
					gap := selRects last height.
					aPivot y < anEvent hand position y
						ifTrue: [ 
							gap := gap + (anEvent hand position y - viewBounds bottom).
							anEvent setPosition: anEvent position + (0 @ gap) ]
						ifFalse: [ 
							gap := gap + (viewBounds top - anEvent hand position y).
							anEvent setPosition: anEvent position - (0 @ gap) ].
					self textArea mouseMove: anEvent ] ]