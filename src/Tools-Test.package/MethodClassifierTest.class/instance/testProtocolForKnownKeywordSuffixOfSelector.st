tests
testProtocolForKnownKeywordSuffixOfSelector
	| classifier |
	classifier := MethodClassifier new.
	classifier class keywordSuffixMapping
		keysAndValuesDo: [ :suffix :protocol | 
			self
				assert: (classifier protocolForKnownKeywordSuffixOfSelector: suffix asSymbol) = protocol;
				assert: (classifier protocolForKnownKeywordSuffixOfSelector: ('more' , suffix) asSymbol) = protocol.
			(suffix endsWith: ':')
				ifTrue: [
					self
						assert: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'more:') asSymbol) = protocol;
						assert: (classifier protocolForKnownKeywordSuffixOfSelector: ('more' , suffix , 'more:') asSymbol) = protocol.
					self
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix allButLast , 'More') asSymbol) = protocol;
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix allButLast , 'More:') asSymbol) = protocol ]
				ifFalse: [
					self
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'More') asSymbol) = protocol;
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'More:') asSymbol) = protocol ] ]