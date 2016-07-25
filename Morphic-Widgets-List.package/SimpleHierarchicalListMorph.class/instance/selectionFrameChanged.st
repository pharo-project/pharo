updating
selectionFrameChanged
	"Invalidate frame of the current selection if any."
	
	selectedMorph ifNil: [ ^self ].
	self invalidRect: self selectionFrame