testing
needSeparatorBefore: index
	| tmpIndex |
	index <= 1
		ifTrue: [ ^ false ].
	tmpIndex := index.
	self items
		do: [ :elements | 
			elements size >= tmpIndex
				ifTrue: [ ^ tmpIndex = 1 ]
				ifFalse: [ tmpIndex := tmpIndex - elements size ] ].
	self errorSubscriptBounds: index