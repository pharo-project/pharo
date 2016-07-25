support
checkClass: aRBClass selector: aSelector using: aMatcher 
	| parseTree |
	parseTree := aRBClass parseTreeFor: aSelector.
	parseTree notNil ifTrue: [aMatcher executeTree: parseTree].
	^aMatcher answer