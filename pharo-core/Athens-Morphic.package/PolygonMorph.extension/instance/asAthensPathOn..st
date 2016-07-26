*Athens-Morphic
asAthensPathOn: anAthensCanvas
	^ self isCurvy
		ifTrue: [ 
			self isOpen
				ifTrue: [ self asAthensCurvedOpenPathOn: anAthensCanvas ]
				ifFalse: [ self asAthensCurvedPathOn: anAthensCanvas ] ]
		ifFalse: [ self asAthensLinePathOn: anAthensCanvas ]