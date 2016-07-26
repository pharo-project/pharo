comparing
= aRemoveMethodChange
	super = aRemoveMethodChange
		ifFalse: [ ^ false ].
	^ selector = aRemoveMethodChange selector