accessing
childAt: anIndex
	| tmpIndex |
	tmpIndex := anIndex.
	self children
		do:
			[ :item | 
			| size |
			size := item numberOfVisibleChildren + 1.
			tmpIndex <= size
				ifTrue: [ ^ item selfOrChildAt: tmpIndex ]
				ifFalse: [ tmpIndex := tmpIndex - size ] ].
	self errorSubscriptBounds: anIndex