tests
testProtocolForKnownPrefixOfSelector
	| classifier |
	classifier := MethodClassifier new.
	classifier class prefixMapping
		keysAndValuesDo: [ :prefix :protocol | 
			self
				assert: (classifier protocolForKnownPrefixOfSelector: prefix asSymbol) = protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , 'More') asSymbol) = protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , ':') asSymbol) = protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , ':more:') asSymbol) = protocol.

			"it shouldn't mis-classify 'island' as 'testing' just because it starts with 'is'"
			self deny: (classifier protocolForKnownPrefixOfSelector: (prefix , 'more') asSymbol) = protocol ]