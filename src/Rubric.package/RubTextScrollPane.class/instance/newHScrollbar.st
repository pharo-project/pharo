accessing
newHScrollbar
	"Answer a new horizontal scrollbar."

	^RubScrollBar new
		model: self;
		setValueSelector: #hScrollbarValue: