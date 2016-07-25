event handling
mouseDown: evt

	^ evt shiftPressed
		ifTrue: [((owner isKindOf: PolygonMorph) and: [owner includesHandle: self])
					ifTrue: ["Prevent insertion handles from getting edited"
							^ super mouseDown: evt].
				self toggleHandles.
				handles ifNil: [^ self].
				vertices withIndexDo:  "Check for click-to-drag at handle site"
					[:vertPt :vertIndex |
					((handles at: vertIndex*2-1 ifAbsent: [ ^self ]) containsPoint: evt cursorPoint) ifTrue:
						["If clicked near a vertex, jump into drag-vertex action"
						evt hand newMouseFocus: (handles at: vertIndex*2-1)]]]
		ifFalse: [super mouseDown: evt]