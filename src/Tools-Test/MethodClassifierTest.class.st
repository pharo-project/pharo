"
SUnit tests for MethodClassifier
"
Class {
	#name : #MethodClassifierTest,
	#superclass : #TestCase,
	#category : #'Tools-Test-Base'
}

{ #category : #tests }
MethodClassifierTest >> testProtocolForKnownKeywordSuffixOfSelector [
	| classifier |
	classifier := MethodClassifier new.
	classifier class keywordSuffixMapping
		keysAndValuesDo: [ :suffix :protocol | 
			self
				assert: (classifier protocolForKnownKeywordSuffixOfSelector: suffix asSymbol) equals: protocol;
				assert: (classifier protocolForKnownKeywordSuffixOfSelector: ('more' , suffix) asSymbol) equals: protocol.
			(suffix endsWith: ':')
				ifTrue: [
					self
						assert: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'more:') asSymbol) equals: protocol;
						assert: (classifier protocolForKnownKeywordSuffixOfSelector: ('more' , suffix , 'more:') asSymbol) equals: protocol.
					self
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix allButLast , 'More') asSymbol) equals: protocol;
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix allButLast , 'More:') asSymbol) equals: protocol ]
				ifFalse: [
					self
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'More') asSymbol) equals: protocol;
						deny: (classifier protocolForKnownKeywordSuffixOfSelector: (suffix , 'More:') asSymbol) equals: protocol ] ]
]

{ #category : #tests }
MethodClassifierTest >> testProtocolForKnownPrefixOfSelector [
	| classifier |
	classifier := MethodClassifier new.
	classifier class prefixMapping
		keysAndValuesDo: [ :prefix :protocol | 
			self
				assert: (classifier protocolForKnownPrefixOfSelector: prefix asSymbol) equals: protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , 'More') asSymbol) equals: protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , ':') asSymbol) equals: protocol;
				assert: (classifier protocolForKnownPrefixOfSelector: (prefix , ':more:') asSymbol) equals: protocol.

			"it shouldn't mis-classify 'island' as 'testing' just because it starts with 'is'"
			self deny: (classifier protocolForKnownPrefixOfSelector: (prefix , 'more') asSymbol) = protocol ]
]
