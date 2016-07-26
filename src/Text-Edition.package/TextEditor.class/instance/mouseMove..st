events
mouseMove: evt
	"Change the selection in response to mouse-down drag"
	| characterBlock |
	
	characterBlock := self paragraph characterBlockAtPoint: evt cursorPoint.

	self editingState pointBlock: characterBlock.
	self paragraph move: evt for: model controller: self.
	self storeSelectionInParagraph