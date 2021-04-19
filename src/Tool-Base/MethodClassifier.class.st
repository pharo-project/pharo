"
I am a method classifier that sets the protocl of methods using some simple rules.

Example Usage:
	MethodClassifier classify: MyClass >> #mySelector
"
Class {
	#name : #MethodClassifier,
	#superclass : #Object,
	#instVars : [
		'protocol'
	],
	#classInstVars : [
		'prefixMapping',
		'keywordSuffixMapping',
		'pragmaMapping'
	],
	#category : #'Tool-Base-Utilities'
}

{ #category : #private }
MethodClassifier class >> buildKeywordSuffixMapping [
	keywordSuffixMapping := Dictionary new
		at: 'Add:' put: 'adding';
		at: 'AddAll:' put: 'adding';
		at: 'Associations' put: 'accessing';
		at: 'At:' put: 'accessing';
		at: 'Collect:' put: 'enumerating';
		at: 'Detect:' put: 'enumerating';
		at: 'Do:' put: 'enumerating';
		at: 'Includes:' put: 'testing';
		at: 'Keys' put: 'accessing';
		at: 'Reject:' put: 'enumerating';
		at: 'Remove:' put: 'removing';
		at: 'RemoveAll' put: 'removing';
		at: 'RemoveKey:' put: 'removing';
		at: 'Select:' put: 'enumerating';
		at: 'Values' put: 'accessing';
		yourself
]

{ #category : #private }
MethodClassifier class >> buildPragmaMapping [
	pragmaMapping := Dictionary new
		at: 'example' put: 'examples';
		at: 'spec' put: 'specs';
		at: 'spec:' put: 'specs';
		at: 'symbolicVersion:' put: 'symbolic versions';
		yourself
]

{ #category : #private }
MethodClassifier class >> buildPrefixMapping [
	prefixMapping := Dictionary new
		at: 'accept' put: 'visiting';
		at: 'accepts' put: 'testing';
		at: 'add' put: 'adding';
		at: 'as' put: 'converting';
		at: 'assert' put: 'asserting';
		at: 'at' put: 'accessing';
		at: 'baseline' put: 'baselines';
		at: 'bench' put: 'benchmarking';
		at: 'benchmark' put: 'benchmarking';
		at: 'can' put: 'testing';
		at: 'compile' put: 'compiling';
		at: 'copy' put: 'copying';
		at: 'deny' put: 'asserting';
		at: 'first' put: 'accessing';
		at: 'format' put: 'formatting';
		at: 'from' put: 'instance creation';
		at: 'has' put: 'testing';
		at: 'includes' put: 'testing';
		at: 'index' put: 'accessing';
		at: 'initialize' put: 'initialization';
		at: 'is' put: 'testing';
		at: 'last' put: 'accessing';
		at: 'matches' put: 'testing';
		at: 'max' put: 'accessing';
		at: 'maximum' put: 'accessing';
		at: 'min' put: 'accessing';
		at: 'minimum' put: 'accessing';
		at: 'needs' put: 'testing';
		at: 'new' put: 'instance creation';
		at: 'parse' put: 'parsing';
		at: 'print' put: 'printing';
		at: 'remove' put: 'removing';
		at: 'render' put: 'rendering';
		at: 'requires' put: 'testing';
		at: 'reset' put: 'initialization';
		at: 'set' put: 'initialization';
		at: 'should' put: 'asserting';
		at: 'shouldnt' put: 'asserting';
		at: 'signal' put: 'signalling';
		at: 'sort' put: 'sorting';
		at: 'test' put: 'tests';
		at: 'total' put: 'accessing';
		at: 'version' put: 'versions';
		at: 'visit' put: 'visiting';
		at: 'write' put: 'writing';
		yourself
]

{ #category : #classification }
MethodClassifier class >> classify: aMethod [
	^ self new classify: aMethod
]

{ #category : #classification }
MethodClassifier class >> classifyAll: aCollectionOfMethods [
	^ self new classifyAll: aCollectionOfMethods
]

{ #category : #private }
MethodClassifier class >> clearMappings [
	"self clearMappings"

	prefixMapping := nil.
	keywordSuffixMapping := nil.
	pragmaMapping := nil.
]

{ #category : #accessing }
MethodClassifier class >> keywordSuffixMapping [
	"use a class inst var so subclasses can define custom mappings"
	^ keywordSuffixMapping ifNil: [
		self buildKeywordSuffixMapping.
		keywordSuffixMapping]
]

{ #category : #accessing }
MethodClassifier class >> pragmaMapping [
	"use a class inst var so subclasses can define custom mappings"
	^ pragmaMapping ifNil: [
		self buildPragmaMapping.
		pragmaMapping]
]

{ #category : #accessing }
MethodClassifier class >> prefixMapping [
	"use a class inst var so subclasses can define custom mappings"
	^ prefixMapping ifNil: [
		self buildPrefixMapping.
		prefixMapping]
]

{ #category : #classification }
MethodClassifier class >> suggestProtocolFor: aMethod [
	^ (self new suggestProtocolFor: aMethod) ifNil: [ 'as yet unclassified' ]
]

{ #category : #classification }
MethodClassifier >> classify: aMethod [

	self suggestProtocolFor: aMethod.
	protocol ifNotNil: [ aMethod protocol: protocol ]



]

{ #category : #classification }
MethodClassifier >> classifyAll: aCollectionOfMethods [
	
	aCollectionOfMethods do: [ :method |
		self resetProtocol.
		self classify: method ]
]

{ #category : #accessing }
MethodClassifier >> listToFindProtocols [
	^ #(#protocolAccessorFor: #protocolByKnownKeywordSuffix: 
	#protocolByKnownPragma: #protocolByKnownPrefix: 
	#protocolByOtherImplementors: #protocolInSuperclassProtocol:)
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolAccessorFor: aMethod [
	" If the method is a setter or getter for a  "
	| names selector |
	
	names := aMethod methodClass allInstVarNames.
	selector := aMethod selector.
	
	(selector endsWith: ':')
		ifTrue: [ "selector might be a setter"
			selector := selector allButLast ].
	
	(names includes: selector)
		ifFalse: [ ^ self ].
	protocol := 'accessing'
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolByKnownKeywordSuffix: aMethod [
	(self protocolForKnownKeywordSuffixOfSelector: aMethod selector)
		ifNotNil: [ :p | protocol := p ]
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolByKnownPragma: aMethod [
	self class pragmaMapping
		keysAndValuesDo: [ :pragma :p | 
			(aMethod hasPragmaNamed: pragma)
				ifTrue: [ protocol := p] ]
	
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolByKnownPrefix: aMethod [
	(self protocolForKnownPrefixOfSelector: aMethod selector)
		ifNotNil: [ :p | protocol := p ]
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolByOtherImplementors: aMethod [
	| protocolBag |
	
	protocolBag := Bag new.
	
	aMethod implementors 
		ifEmpty: [ ^ self ]
		ifNotEmpty: [ :methods |
			methods 
				do: [ :method | 
					((method protocol beginsWith: '*') or: [ method protocol = Protocol unclassified ])
						ifFalse: [ protocolBag add: method protocol ]]
				without: aMethod ].
			
	protocolBag ifEmpty: [ ^ self ].
	protocol := protocolBag sortedCounts first value.
]

{ #category : #private }
MethodClassifier >> protocolForKnownKeywordSuffixOfSelector: aSelector [
	"Returns the protocol for the first keyword suffix that equals a keyword
	from aSelector or that a keyword ends with or nil if there isn't one."

	aSelector keywords
		do: [ :each | 
			self class keywordSuffixMapping
				keysAndValuesDo: [ :suffix :p | 
					(each = suffix or: [ each endsWith: suffix ])
						ifTrue: [ ^ p ] ] ].
	^ nil
]

{ #category : #private }
MethodClassifier >> protocolForKnownPrefixOfSelector: aSelector [
	"Returns the protocol for the first prefix that equals aSelector or that
	aSelector starts with if the prefix is followed by a non-lowercase char
	(like $: or $A) or nil if there isn't one."

	| selectorString |
	selectorString := aSelector asString.
	self class prefixMapping
		keysAndValuesDo: [ :prefix :p | 
			(selectorString = prefix
				or: [ (selectorString beginsWith: prefix)
						and: [ (selectorString at: prefix size + 1) isLowercase not ] ])
				ifTrue: [ ^ p ] ].
	^ nil
]

{ #category : #'classification-rules' }
MethodClassifier >> protocolInSuperclassProtocol: aMethod [

	protocol := self superclassProtocol: aMethod
]

{ #category : #reseting }
MethodClassifier >> resetProtocol [
	protocol := nil
]

{ #category : #'smartSuggestions - support' }
MethodClassifier >> suggestProtocolFor: aMethod [ 
	| listToSearch index |
	listToSearch := self listToFindProtocols.
	index := 1.
	[ protocol isNil and: [ index <= listToSearch size ] ] 
	whileTrue: [ self perform: (listToSearch at: index) withArguments: {aMethod}.
		index := index + 1 ].
	^ protocol
]

{ #category : #classification }
MethodClassifier >> superclassProtocol: aMethod [
	| currentClass |
	currentClass := aMethod methodClass.
	
	[ currentClass superclass isNil ] whileFalse: [
		currentClass := currentClass superclass.
		(currentClass includesSelector: aMethod selector) 
			ifTrue: [ 
				| possibleProtocol |
				possibleProtocol := (currentClass >> aMethod selector) protocol.
				((possibleProtocol beginsWith: '*') or: 
					(possibleProtocol asSymbol = currentClass package name))
						ifFalse: [ ^ possibleProtocol ]]].
	^ nil.
]
