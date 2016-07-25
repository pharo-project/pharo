accessing
numberOfVisibleChildren
	^ (self isExpanded and: [ self children isNotEmpty ])
		ifTrue: [ self children size + (self children collect: [ :item | item numberOfVisibleChildren ]) sum ]
		ifFalse: [ 0 ]