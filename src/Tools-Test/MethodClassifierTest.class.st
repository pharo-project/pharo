Class {
	#name : #MethodClassifierTest,
	#superclass : #TestCase,
	#category : 'Tools-Test-Base'
}

{ #category : #tests }
MethodClassifierTest >> testProtocolForKnownKeywordSuffixOfSelector [
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
]

{ #category : #tests }
MethodClassifierTest >> testProtocolForKnownPrefixOfSelector [
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
]
