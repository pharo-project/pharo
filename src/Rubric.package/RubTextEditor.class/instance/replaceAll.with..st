find-select
replaceAll: aRegex with: aText
	self
		undoRedoTransaction: [ 
			| selec ranges |
			selec := self selectionInterval.
			ranges := self findAll: aRegex startingAt: selec first.
			ranges
				reverseDo: [ :r | 
					self selectInvisiblyFrom: r first to: r last.
					self replaceSelectionWith: aText ].
			self selectInterval: selec ]