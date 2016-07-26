private
setOtherSelection: otherOrNil 
	otherSelection := otherOrNil.
	otherOrNil isNil 
		ifTrue: [super borderColor: Color blue]
		ifFalse: 
			[itemsAlreadySelected := otherSelection selectedItems.
			super borderColor: Color green]