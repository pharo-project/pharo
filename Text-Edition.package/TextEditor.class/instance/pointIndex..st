accessing-selection
pointIndex: anIndex
	self editingState pointBlock: (self paragraph characterBlockForIndex: anIndex)
