ui-building
newContentMorph 
	| sp |
	sp := super newContentMorph.
	sp
		updateScrollbars.
	^ sp
