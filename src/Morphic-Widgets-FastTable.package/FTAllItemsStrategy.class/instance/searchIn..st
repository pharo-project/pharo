action
searchIn: item
	| selfOrChildMatch |
	selfOrChildMatch := false.
	(self isMatching: item)
		ifTrue: [ result add: index.
			selfOrChildMatch := true ].
	index := index + 1.
	(item hasChildren and: [ dataSource canDisplayChildrenOf: item ])
		ifTrue:
			[ | itemIndex |
			itemIndex := index.	"I save this index because if there is no match inside this item I should not expand it."
			(item children select: [ :child | self searchIn: child ])
				ifNotEmpty: [ selfOrChildMatch := true.
					item expand ]
				ifEmpty: [ item collapse.
					index := itemIndex ] ].
	^ selfOrChildMatch