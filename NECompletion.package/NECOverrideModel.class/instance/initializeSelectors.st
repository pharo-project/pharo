initialize-release
initializeSelectors
	clazz superclass ifNotNil: [ self initializeSelectorsFor: clazz superclass ].
	clazz
		selectorsDo: [ :each | selectors detect: [ :ea | ea contentsAsSymbol == each ] ifFound: [ :entry | selectors remove: entry ifAbsent: [  ] ] ]