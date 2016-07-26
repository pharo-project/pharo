geometry
extent: newExtent
	
	| oldW oldH wasHShowing wasVShowing noVPlease noHPlease minH minW |
	
	oldW := self width.
	oldH := self height.
	wasHShowing := self hIsScrollbarShowing.
	wasVShowing := self vIsScrollbarShowing.

	"Figure out the minimum width and height for this pane so that scrollbars will appear"
	noVPlease := self valueOfProperty: #noVScrollBarPlease ifAbsent: [false]. 
	noHPlease := self valueOfProperty: #noHScrollBarPlease ifAbsent: [false]. 
	minH := self scrollBarThickness + 16.
	minW := self scrollBarThickness + 20.
	noVPlease ifTrue:[ 
		noHPlease
			ifTrue:[minH := 1. minW := 1 ]
			ifFalse:[minH := self scrollBarThickness ].
	] ifFalse:[
		noHPlease
			ifTrue:[minH := self scrollBarThickness + 5].
	].
	super extent: (newExtent max: (minW@minH)).

	"Now reset widget sizes"
	self resizeScrollBars; resizeScroller; hideOrShowScrollBars.
	
	"Now resetScrollDeltas where appropriate, first the vScrollBar..."
	((self height ~~ oldH) or: [ wasHShowing ~~ self hIsScrollbarShowing]) ifTrue:
		[self vIsScrollbarShowing ifTrue: [ self vSetScrollDelta ]].
			
	"...then the hScrollBar"
	((self width ~~ oldW) or: [wasVShowing ~~ self vIsScrollbarShowing]) ifTrue:
		[self hIsScrollbarShowing ifTrue: [ self hSetScrollDelta ]].

