updating
updateSelectionWithCollectBlock: aBlock
	"I take a block with two parameters, the index of a selected element and 
	the index of the item that collapsed/expanded. This block needs to return 
	the index of the new selected element. I do not ensure that any of the
	newly selected elements are visible. Use #ensureVisibleFirstSelection for that."

	| index |
	self table selectedRowIndexes ifEmpty: [ ^ self ].
	
	index := self indexOfChangedItem.
	self table 
		selectRowIndexes: ((self table selectedRowIndexes collect: [ :ind | 
			aBlock value: ind value: index ]) asSet) asArray 
		andMakeVisibleIf: false