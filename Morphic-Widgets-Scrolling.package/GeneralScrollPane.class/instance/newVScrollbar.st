instance creation
newVScrollbar
	"Answer a new vertical scrollbar."

	^GeneralScrollBar new
		model: self;
		setValueSelector: #vScrollbarValue: