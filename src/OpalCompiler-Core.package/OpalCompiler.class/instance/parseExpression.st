private
parseExpression
	| parseClass |
	
	parseClass := self compilationContext parserClass.

	^self compilationContext optionParseErrors 
		ifTrue: [parseClass parseFaultyExpression: source contents]
		ifFalse: [parseClass parseExpression: source contents]