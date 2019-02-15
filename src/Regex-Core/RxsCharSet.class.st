"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A character set corresponds to a [...] construct in the regular expression.

Instance variables:
	elements	<OrderedCollection> An element can be one of: RxsCharacter, RxsRange, or RxsPredicate.
	negated		<Boolean>
"
Class {
	#name : #RxsCharSet,
	#superclass : #RxsNode,
	#instVars : [
		'negated',
		'elements'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #accessing }
RxsCharSet >> dispatchTo: aMatcher [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aMatcher syntaxCharSet: self
]

{ #category : #privileged }
RxsCharSet >> enumerablePartPredicateIgnoringCase: aBoolean [

	| enumeration |
	enumeration := self optimalSetIgnoringCase: aBoolean.
	^negated
		ifTrue: [[:char | (enumeration includes: char) not]]
		ifFalse: [[:char | enumeration includes: char]]
]

{ #category : #privileged }
RxsCharSet >> enumerableSetIgnoringCase: aBoolean [
	"Answer a collection of characters that make up the portion of me
	that can be enumerated."

	| set |
	set := Set new.
	elements do:
		[:each |
		each isEnumerable ifTrue:
			[each enumerateTo: set ignoringCase: aBoolean]].
	^set
]

{ #category : #accessing }
RxsCharSet >> hasPredicates [

	^elements contains: [:some | some isEnumerable not]
]

{ #category : #initialization }
RxsCharSet >> initializeElements: aCollection negated: aBoolean [
	"See class comment for instance variables description."

	elements := aCollection.
	negated := aBoolean
]

{ #category : #testing }
RxsCharSet >> isEnumerable [

	^elements anySatisfy: [:some | some isEnumerable ]
]

{ #category : #testing }
RxsCharSet >> isNegated [

	^negated
]

{ #category : #privileged }
RxsCharSet >> optimalSetIgnoringCase: aBoolean [
	"Assuming the client with search the `set' using #includes:,
	answer a collection with the contents of `set', of the class
	that will provide the fastest lookup. Strings are faster than
	Sets for short strings."

	| set |
	set := self enumerableSetIgnoringCase: aBoolean.
	^set size < 10
		ifTrue: [set asArray]
		ifFalse: [set]
]

{ #category : #accessing }
RxsCharSet >> predicateIgnoringCase: aBoolean [

	| predicate enumerable |
	enumerable := self enumerablePartPredicateIgnoringCase: aBoolean.
	^self hasPredicates
		ifFalse: [enumerable]
		ifTrue:
			[predicate := self predicatePartPredicate.
			negated
				ifTrue: [[:char | (enumerable value: char) and: [predicate value: char]]]
				ifFalse: [[:char | (enumerable value: char) or: [predicate value: char]]]]
]

{ #category : #privileged }
RxsCharSet >> predicatePartPredicate [
	"Answer a predicate that tests all of my elements that cannot be
	enumerated."

	| predicates |
	predicates := elements reject: [:some | some isEnumerable].
	predicates isEmpty
		ifTrue: [^[:char | negated]].
	predicates size = 1
		ifTrue: [^negated
			ifTrue: [predicates first predicateNegation]
			ifFalse: [predicates first predicate]].
	predicates := predicates collect: [:each | each predicate].
	^negated
		ifFalse:
			[[:char | predicates contains: [:some | some value: char]]]
		ifTrue:
			[[:char | (predicates contains: [:some | some value: char]) not]]
]

{ #category : #accessing }
RxsCharSet >> predicates [

	^(elements reject: [:some | some isEnumerable])
		collect: [:each | each predicate]
]
