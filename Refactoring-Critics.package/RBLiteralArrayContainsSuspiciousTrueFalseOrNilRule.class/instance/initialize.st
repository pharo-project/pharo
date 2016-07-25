initialization
initialize
	super initialize.
	matcher := RBParseTreeSearcher new.
	matcher 
		matches: '`#array `{ :node | node isLiteralArray and: [ node isForByteArray not ] }'
		do: [ :node :answer | 
			answer 
				addAll: (self literalTrueFalseOrNilSymbolsIn: node value);
				yourself ]