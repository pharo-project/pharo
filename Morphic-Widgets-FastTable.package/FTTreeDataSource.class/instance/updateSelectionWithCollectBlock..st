updating
updateSelectionWithCollectBlock: aBlock
	"I take a block with three parameters, the index of a selected element, the index of the item that collapsed/expanded and the number of visible row of the item that changed. This block need to return the index of the new selected element."

	| index |
	self table selectedRowIndexes ifEmpty: [ ^ self ].
	
	index := self indexOfChangedItem.
	self table selectRowIndexes: ((self table selectedRowIndexes collect: [ :ind | aBlock value: ind value: index ]) asSet) asArray