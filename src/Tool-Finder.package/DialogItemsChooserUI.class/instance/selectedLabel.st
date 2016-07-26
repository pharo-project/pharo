accessing
selectedLabel
	^self dialogItemsChooser isNil
		ifTrue: ['Selected Items' translated]
		ifFalse: [self dialogItemsChooser selectedLabel]