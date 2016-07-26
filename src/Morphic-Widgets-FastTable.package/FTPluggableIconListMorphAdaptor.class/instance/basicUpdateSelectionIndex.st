updating
basicUpdateSelectionIndex
	| rowIndex |
	self getIndexSelector ifNil: [ ^ self ].
	
	rowIndex := self model perform: self getIndexSelector.
	rowIndex = self selectedRowIndex ifTrue: [ ^ self ].
	rowIndex = 0
		ifTrue: [ self basicSelectRowIndexes: #() ]
		ifFalse: [ self basicSelectRowIndexes: { rowIndex } ]