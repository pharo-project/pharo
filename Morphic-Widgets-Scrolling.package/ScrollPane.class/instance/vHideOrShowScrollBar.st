scrolling
vHideOrShowScrollBar

	self vIsScrollbarNeeded
		ifTrue:[ self vShowScrollBar ]
		ifFalse:[ self vHideScrollBar ].
