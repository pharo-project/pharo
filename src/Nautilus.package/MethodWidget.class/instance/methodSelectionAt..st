selection
methodSelectionAt: anIndex
	| elt |
	elt := self getMethods at: anIndex ifAbsent: [ ^ false ].
	^ methodsSelection
		detect: [ :meth | meth methodClass = elt methodClass and: [ meth selector = elt selector ] ]
		ifFound: [ :meth | meth ]
		ifNone: [ false ]