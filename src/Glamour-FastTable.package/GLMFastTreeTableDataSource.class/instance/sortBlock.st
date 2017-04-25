sorting
sortBlock
	^  columnToSortBy sortBlock ifNil: [ |defaultSortBlock|
			defaultSortBlock := columnToSortBy defaultSortBlockForPresentation: self glamourPresentation.
		 	[ :x :y | defaultSortBlock value: x data value: y data ] ]