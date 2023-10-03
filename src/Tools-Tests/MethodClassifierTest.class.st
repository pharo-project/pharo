"
SUnit tests for MethodClassifier
"
Class {
	#name : 'MethodClassifierTest',
	#superclass : 'TestCase',
	#category : 'Tools-Tests-Base',
	#package : 'Tools-Tests',
	#tag : 'Base'
}

{ #category : 'tests' }
MethodClassifierTest >> testOverridenMethodProtocolHasPriority [

	"We want to check that the superclass method protocol has the priority so let's ensure no one messed with the mock first."
	self assert: (MethodClassifierMockSuperclass >> #initializeNotInInitialize) protocolName equals: #protocol.

	"To test that the priority works, we need to make sure that another protocol suggestion strategy works"
	self assert: (MethodClassifier new protocolByKnownPrefix: MethodClassifierMock >> #initializeNotInInitialize) protocol equals: #initialization.

	"And now we check that the behavior we want has the priority!"
	self assert: (MethodClassifier new suggestProtocolFor: MethodClassifierMock >> #initializeNotInInitialize) equals: #protocol
]

{ #category : 'tests' }
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

{ #category : 'tests' }
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
			self deny: (classifier protocolForKnownPrefixOfSelector: (prefix , 'more') asSymbol) equals: protocol ]
]

{ #category : 'tests' }
MethodClassifierTest >> testProtocolForSetUp [

	self assert: (MethodClassifier new suggestProtocolFor: ClassTest >> #setUp) equals: #running
]

{ #category : 'tests' }
MethodClassifierTest >> testProtocolForTearDown [

	self assert: (MethodClassifier suggestProtocolFor: ClassTest >> #tearDown) equals: #running
]
