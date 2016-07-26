instance creation
definition: aString for: aController
	| parseTree context |
	parseTree := RBParser
		parseExpression: aString
		onError: [ :err :pos | ^ self error: 'Invalid definition string' ].
	context := SmallDictionary new.
	RBRefactoryDefinitionChange allSubclassesDo: [ :class |
		class definitionPatterns do: [ :pattern |
			((RBParser parseRewriteExpression: pattern) match: parseTree inContext: context empty)
			 	ifTrue: [ ^ class definition: aString for: aController context: context ] ] ].
	^ self error: 'Invalid definition string'