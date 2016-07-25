text area protocol
scrollSelectionIntoView: event
	"Scroll my text into view if necessary and return true, else return false"

	| selRects delta selRect rectToTest transform cpHere editor bnds |
	editor := self textArea editor.
	selRects := self textArea selectionRects.
	selRects isEmpty
		ifTrue: [ ^ false ].
	rectToTest := selRects first merge: selRects last.
	transform := scroller transformFrom: self.
	(event notNil and: [ event isMouse and: [ event anyButtonPressed ] ])
		ifTrue: [ 
			"Check for autoscroll"
			cpHere := transform localPointToGlobal: event cursorPoint.
			(cpHere y <= self top or: [ cpHere x <= self left ])
				ifTrue: [ rectToTest := selRects first topLeft extent: 2 @ 2 ]
				ifFalse: [ 
					(cpHere y >= self bottom or: [ cpHere x >= self right  ] )
						ifTrue: [ rectToTest := selRects last bottomRight extent: 2 @ 2 ]
						ifFalse: [ ^ false ] ] ].
	selRect := transform localBoundsToGlobal: rectToTest.
	bnds := self scrollBounds insetBy: self textArea margins.
	selRect height > bounds height
		ifTrue: [ 
			editor pointIndex - editor markIndex < 0
				ifTrue: [ self scrollBy: 0 @ (bnds top - selRect top) ]
				ifFalse: [ self scrollBy: 0 @ (bnds bottom - selRect bottom) ].
			^ true ].
	(delta := selRect amountToTranslateWithin: bnds) ~= (0 @ 0)
		ifTrue: [ 
			delta x > 0
				ifTrue: [ 
					scroller offset x - delta x < self scrollBounds width
						ifTrue: [ delta := scroller offset x @ delta y ] ].
			self scrollBy: delta truncated.
			^ true ].
	^ false