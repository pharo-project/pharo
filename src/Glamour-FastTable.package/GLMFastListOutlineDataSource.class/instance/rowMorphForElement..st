factory
rowMorphForElement: element
	| rowElements |
	rowElements := OrderedCollection new.
	(self iconFor: element) ifNotNil: [ :icon | rowElements add: icon asMorph ].
	rowElements
		add: (self formatedDisplayValueOf: element) asMorph;
		addAll: (self tagMorphsFrom: self glamourPresentation for: element).
	^ Smalltalk ui theme newRowIn: World for: rowElements