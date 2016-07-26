queries
whichSelectorsRead: aString
	
	| index |
	index := self
		instVarIndexFor: aString
		ifAbsent: [ ^ #() ].
	^ self selectors select: [ :each |
		(self compiledMethodAt: each)
			readsField: index ]