button
generateButton
	^ isExpanded
		ifTrue: [ self expandedButton ]
		ifFalse: [ self collpasedButton ]