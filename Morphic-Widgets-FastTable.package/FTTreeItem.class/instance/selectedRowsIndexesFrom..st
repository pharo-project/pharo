updating
selectedRowsIndexesFrom: anInteger
	| index selfSelection |
	index := anInteger.
	self recentlyChanged
		ifTrue: [ ^ (self dataSource table selectedRowIndexes select: [ :each | each between: index and: index + self numberOfVisibleChildren ]) ifEmpty: [ {} ] ifNotEmpty: [ {index} ] ].
	selfSelection := (self dataSource table selectedRowIndexes includes: index)
		ifTrue: [ {index} ]
		ifFalse: [ {} ].
	self isExpanded
		ifFalse: [ ^ selfSelection ].
	index := index + 1.
	^ (self children
		flatCollect: [ :child | 
			| result |
			result := child selectedRowsIndexesFrom: index.
			index := index + 1 + child numberOfVisibleChildren.
			result ]) , selfSelection