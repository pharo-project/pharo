scrollbar-calculations
approximateBandHeight
	|visible averageItemHeight|
	
	visible := self scrollPane band submorphs select: [ :each | self scrollPane isPartiallyVisible: each ].
	visible isEmpty ifTrue: [ ^ 0 ].
	
	averageItemHeight := (visible inject: 0 into: [ :visibleHeight :each | visibleHeight + each height ]) / visible size.
	^ self scrollPane model count * averageItemHeight.