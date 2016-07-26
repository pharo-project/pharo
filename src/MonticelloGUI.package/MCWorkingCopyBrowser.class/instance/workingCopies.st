morphic ui
workingCopies
	^ (self orderSpecs size = order
		ifTrue: [ self allManagers select: [ :each | each modified ] ]
		ifFalse: [ self allManagers ])
			asSortedCollection: (self orderSpecs at: order) value.