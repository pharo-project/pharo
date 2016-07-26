accessing
selectionParseTreeIntervalFor: aString 
	| parseTree answerBlock |
	parseTree := RBParser
		parseMethod: aString
		onError: [ :str :pos | ^ nil ].
	answerBlock := [ :aNode :answer | ^ aNode sourceInterval ].
	self searchStrings do: [ :each | 
		| matcher tree |
		matcher := RBParseTreeSearcher new.
		matcher matchesTree: (RBLiteralNode value: each) do: answerBlock.
		each isSymbol 
			ifTrue: [
				tree := RBParseTreeSearcher buildSelectorTree: each.
				tree notNil ifTrue: [ matcher matchesTree: tree do: answerBlock ] ]
			ifFalse: [
				tree := RBVariableNode named: each.
				matcher
					matchesTree: tree do: answerBlock;
					matchesArgumentTree: tree do: answerBlock ].
		matcher executeTree: parseTree ].
	^ nil