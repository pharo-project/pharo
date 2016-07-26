private
parseMethod
	| parseClass |
	
	parseClass := self compilationContext parserClass.

	^self compilationContext optionParseErrors 
		ifTrue: [parseClass parseFaultyMethod: source contents]
		ifFalse: [parseClass parseMethod: source contents]