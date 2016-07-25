scrolling
scrollSelectionIntoView
	"make sure that the current selection is visible"
	self listManager selectedMorph
		ifNotNil: [:morph | self scrollToShow: morph bounds ]