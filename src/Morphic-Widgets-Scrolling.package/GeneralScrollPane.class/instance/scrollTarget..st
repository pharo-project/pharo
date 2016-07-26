scrollbars
scrollTarget: aMorph
	"Set the morph that is scrolled."
	
	self scroller
		removeAllMorphs;
		addMorph: aMorph.
	self updateScrollbars