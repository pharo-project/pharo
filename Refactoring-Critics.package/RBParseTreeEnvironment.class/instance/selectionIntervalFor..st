accessing
selectionIntervalFor: aString 
	| parseTree node |
	matcher isNil 
		ifTrue: [ ^ super selectionIntervalFor: aString ].
	parseTree := RBParser
		parseMethod: aString
		onError: [ :error :position | ^ super selectionIntervalFor: aString ].
	node := matcher executeTree: parseTree initialAnswer: nil.
	^ (node isKindOf: RBProgramNode)
		ifTrue: [ node sourceInterval ]
		ifFalse: [ super selectionIntervalFor: aString ]