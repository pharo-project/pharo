find-select
replaceAll: aRegex with: aText
	| selec ranges |
	self editingState undoManager openRecordGroup.
	selec := self selectionInterval.
	ranges := self findAll: aRegex startingAt: selec first. 
	ranges reverseDo: [:r | self selectInvisiblyFrom: r first to: r last.
		self replaceSelectionWith: aText].
	self selectInterval: selec.
	self editingState undoManager closeRecordGroup.
