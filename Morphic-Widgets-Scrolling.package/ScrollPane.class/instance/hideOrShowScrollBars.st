scrolling
hideOrShowScrollBars

	| wasHShowing wasVShowing |

	wasVShowing := self vIsScrollbarShowing.
	wasHShowing := self hIsScrollbarShowing.

	self 
		vHideOrShowScrollBar; 
		hHideOrShowScrollBar; 
		resizeScrollBars.

	(wasVShowing and: [self vIsScrollbarShowing not]) ifTrue:
		["Make sure the delta is 0"
		(scroller offset y = 0) 
				ifFalse:[ scroller offset: (scroller offset x@0) ]].
			
	(wasHShowing and: [self hIsScrollbarShowing not]) ifTrue:
		[(scroller offset x <= 0)
				ifFalse:[ scroller offset: (self hMargin negated@scroller offset y)]].
