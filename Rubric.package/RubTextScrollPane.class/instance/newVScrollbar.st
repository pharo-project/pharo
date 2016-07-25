accessing
newVScrollbar
	"Answer a new vertical scrollbar."

	^RubScrollBar new
		model: self;
		setValueSelector: #vScrollbarValue: