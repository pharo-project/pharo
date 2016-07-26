as yet unclassified
selectNextState
	"Set the selection state to the next one with wraparound."

	self selectionState: (
		self selectionStates at: (
			(self selectionStates indexOf: self selectionState) \\
				self selectionStates size + 1))