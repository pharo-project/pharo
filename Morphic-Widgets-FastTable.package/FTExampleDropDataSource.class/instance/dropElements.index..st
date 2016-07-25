drag and drop
dropElements: aCollection index: rowIndex
	self elements: (rowIndex = 0
		ifTrue: [ self elements, aCollection ]
		ifFalse: [ (self elements first: rowIndex), aCollection, (self elements allButFirst: rowIndex) ]).
	