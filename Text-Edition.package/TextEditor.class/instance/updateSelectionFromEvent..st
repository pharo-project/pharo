private
updateSelectionFromEvent: event

	| clickPoint b | 

	clickPoint := event cursorPoint.
	b := self paragraph characterBlockAtPoint: clickPoint.
	
	self editingState markBlock: b.
	self editingState pointBlock: b.
	
	self storeSelectionInParagraph