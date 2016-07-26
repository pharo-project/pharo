actions
addIfNeededTitle: aTitle andIcon: anIcon

	(aTitle notNil or: [ anIcon notNil ])
		ifTrue: [ self addTitle: aTitle icon: anIcon ]