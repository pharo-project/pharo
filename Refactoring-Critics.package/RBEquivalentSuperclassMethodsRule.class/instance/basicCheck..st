running
basicCheck: aMethod
	(self ignoredSelectors includes: aMethod selector)
		ifTrue: [ ^ false ].
	aMethod methodClass superclass
		ifNil: [ ^ false ]
		ifNotNil: [ :superclass |
			(superclass lookupSelector: aMethod selector)
			ifNil: [ ^ false ]
			ifNotNil: [ :overridenMethod |
				^ aMethod ast = overridenMethod ast ] ]