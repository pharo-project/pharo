scrolling
hScrollBarValue: scrollValue
	| x systemWindow |
	
	self hIsScrollbarShowing ifFalse: 
		[ ^ scroller offset: (0 - self hMargin)@scroller offset y ].
	((x := self hLeftoverScrollRange * scrollValue) <= 0)
		ifTrue:[ x := 0 - self hMargin ].
	scroller offset: (x@scroller offset y).

	owner ifNil: [ ^ self ].
	
	systemWindow := self window.
	systemWindow ifNotNil:
		[ systemWindow 
			announce: (WindowScrolling new step: scrollValue@0; window: systemWindow) ].
		
	self announcer announce: (PaneScrolling new 
		step: scrollValue@0;
		scrollPane: self;
		yourself)