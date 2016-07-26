private
hScrollBarValue: scrollValue
	super hScrollBarValue: scrollValue.
	textMorph ifNotNil: [textMorph selectionChanged].
	
	self triggerEvent: #hScroll with: scrollValue