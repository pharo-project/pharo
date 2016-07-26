accessing
unselectedLabel
	^self dialogItemsChooser isNil
		ifTrue: ['Unselected Items' translated]
		ifFalse: [self dialogItemsChooser unselectedLabel]