action
realSearch
	| result index |
	result := OrderedCollection new.
	index := 1.
	dataSource rootsItems
		do: [ :item | 
			(self isMatching: item)
				ifTrue: [ result add: index ].
			index := index + (item numberOfVisibleChildren + 1) ].
	^ result asArray