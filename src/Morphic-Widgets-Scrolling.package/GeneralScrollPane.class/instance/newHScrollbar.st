instance creation
newHScrollbar
	"Answer a new horizontal scrollbar."

	^GeneralScrollBar new
		model: self;
		setValueSelector: #hScrollbarValue: