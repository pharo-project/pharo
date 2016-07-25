find-select
replaceAll: aRegex with: aText startingAt: startIdx
	self
		undoRedoTransaction: [ 
			| selec ranges |
			selec := self selectionInterval.
			ranges := self findAll: aRegex startingAt: startIdx.
			ranges
				reverseDo: [ :r | 
					self selectInvisiblyFrom: r first to: r last.
					self replaceSelectionWith: aText ].
			self selectInterval: selec ]