accessing
elementAt: index
	| tmpIndex |
	tmpIndex := index.
	self items
		do: [ :elements | 
			tmpIndex <= elements size
				ifTrue: [ ^ elements at: tmpIndex ]
				ifFalse: [ tmpIndex := tmpIndex - elements size ] ].
	self errorSubscriptBounds: index