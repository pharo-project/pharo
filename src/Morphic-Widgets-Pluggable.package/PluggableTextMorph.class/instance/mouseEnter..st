event handling
mouseEnter: event
	"Changed to take keyboardFocusOnMouseDown into account."
	
	super mouseEnter: event.
	self textMorph ifNil: [^self].
	selectionInterval ifNotNil:
		[self textMorph editor selectInterval: selectionInterval; setEmphasisHere].
	self textMorph selectionChanged.
	self wantsKeyboardFocus ifFalse: [^self].
	self keyboardFocusOnMouseDown
		ifFalse: [self textMorph takeKeyboardFocus]