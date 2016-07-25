action
realSearch
	| result cpt |
	result := OrderedCollection new.
	cpt := 0.
	dataSource rootsItems
		do:
			[ :item | 
			1 to: item numberOfVisibleChildren + 1 do: [ :index | 
				| elem |
				elem := item selfOrChildAt: index.
				(self isMatching: elem)
					ifTrue: [ result add: index + cpt ] ].
			cpt := cpt + (item numberOfVisibleChildren + 1) ].
	^ result asArray