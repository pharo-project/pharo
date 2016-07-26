actions
resizeCursor

	^ Cursor resizeForEdge: (splitsTopAndBottom
		ifTrue: [#top]
		ifFalse: [#left])
		