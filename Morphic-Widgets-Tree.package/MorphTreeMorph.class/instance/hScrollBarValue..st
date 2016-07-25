scrolling
hScrollBarValue: scrollValue
	| prev |
	prev := scroller offset x.
	super hScrollBarValue: scrollValue.
	scroller offset x ~= prev
		ifTrue: [
			self updateColumnResizersXOffset.
			self updateTopHeader]