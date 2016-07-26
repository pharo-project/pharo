method accessing
selectors
	| selectors |
	selectors := Set new.
	selectors addAll: self newMethods keys.
	self isDefined 
		ifTrue: 
			[selectors addAll: self realClass selectors.
			removedMethods notNil 
				ifTrue: [removedMethods do: [:each | selectors remove: each ifAbsent: []]]].
	^selectors