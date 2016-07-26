accessing selection
highlightRowIndexes: anArray
	anArray = highlightedRowIndexes ifTrue: [ ^ self ].
	
	self basicHighlightRowIndexes: anArray. 
	
	(self hasHighlighted and: [ (self isRowIndexVisible: self highlightedRowIndex) not ]) 
		ifTrue: [ 
			self moveShowIndexTo: self highlightedRowIndexes first.
			^ self ].
	
	(self hasSelection and: [ (self isRowIndexVisible: self selectedRowIndex) not  ])
		ifTrue: [ 
			self moveShowIndexTo: self selectedRowIndex.
			^ self ].
	
	self refresh.