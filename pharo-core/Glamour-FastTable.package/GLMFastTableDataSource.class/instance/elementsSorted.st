private
elementsSorted
	elements ifNil: [ ^ nil ].
	^ columnToSortBy 
		ifNotNil: [ elements sorted: self sortBlock ]
		ifNil: [ self calculateElements ]