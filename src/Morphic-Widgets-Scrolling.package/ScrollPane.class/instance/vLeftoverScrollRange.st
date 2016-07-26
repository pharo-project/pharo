geometry
vLeftoverScrollRange
	"Return the entire scrolling range minus the currently viewed area."
	scroller hasSubmorphs ifFalse:[^0].
	^ (self vTotalScrollRange -  self vScrollBarHeight roundTo: self scrollDeltaHeight) max: 0
