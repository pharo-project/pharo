initialization
initializeScrollBars
	"initialize the receiver's scrollBar"

	(scrollBar := ScrollBar new model: self; setValueSelector: #vScrollBarValue:)
		borderWidth: 1;
		borderColor: Color black.
	(hScrollBar := ScrollBar new model: self; setValueSelector: #hScrollBarValue:)
		borderWidth: 1;
		borderColor: Color black.
	self initializeScroller.
	self addMorph: scrollBar.
	self addMorph: hScrollBar.
	self alwaysShowVScrollbar
		ifTrue: [ self alwaysShowVScrollBar: true ].
	self alwaysHideHScrollbar
		ifTrue: [ self hideHScrollBarIndefinitely: true ]
		ifFalse: [ 
			self alwaysShowHScrollbar
				ifTrue: [ self alwaysShowHScrollBar: true ] ]