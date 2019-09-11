"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A piece is an atom, possibly optional or repeated a number of times.

Instance variables:
	atom	<RxsCharacter|RxsCharSet|RxsPredicate|RxsRegex|RxsSpecial>
	min		<Integer>
	max		<Integer|nil> nil means infinity
"
Class {
	#name : #RxsPiece,
	#superclass : #RxsNode,
	#instVars : [
		'atom',
		'min',
		'max'
	],
	#category : #'Regex-Core-Nodes'
}

{ #category : #accessing }
RxsPiece >> atom [

	^atom
]

{ #category : #accessing }
RxsPiece >> character [
	"If this node is atomic, answer the character it
	represents. It is the caller's responsibility to make sure this
	node is indeed atomic before using this."

	^atom character
]

{ #category : #accessing }
RxsPiece >> dispatchTo: aMatcher [
	"Inform the matcher of the kind of the node, and it
	will do whatever it has to."

	^aMatcher syntaxPiece: self
]

{ #category : #initialization }
RxsPiece >> initializeAtom: anAtom [
	"This piece is exactly one occurrence of the specified RxsAtom."

	self initializeAtom: anAtom min: 1 max: 1
]

{ #category : #initialization }
RxsPiece >> initializeAtom: anAtom min: minOccurrences max: maxOccurrences [
	"This piece is from <minOccurrences> to <maxOccurrences> 
	occurrences of the specified RxsAtom."

	atom := anAtom.
	min := minOccurrences.
	max := maxOccurrences
]

{ #category : #initialization }
RxsPiece >> initializeOptionalAtom: anAtom [
	"This piece is 0 or 1 occurrences of the specified RxsAtom."

	self initializeAtom: anAtom min: 0 max: 1
]

{ #category : #'initialize-release' }
RxsPiece >> initializePlusAtom: anAtom [
	"This piece is one or more occurrences of the specified RxsAtom."

	self initializeAtom: anAtom min: 1 max: nil
]

{ #category : #initialization }
RxsPiece >> initializeStarAtom: anAtom [
	"This piece is any number of occurrences of the atom."

	self initializeAtom: anAtom min: 0 max: nil
]

{ #category : #testing }
RxsPiece >> isAtomic [
	"A piece is atomic if only it contains exactly one atom
	which is atomic (sic)."

	^self isSingular and: [atom isAtomic]
]

{ #category : #testing }
RxsPiece >> isNullable [
	"A piece is nullable if it allows 0 matches. 
	This is often handy to know for optimization."

	^min = 0 or: [atom isNullable]
]

{ #category : #testing }
RxsPiece >> isOptional [

	^min = 0 and: [max = 1]
]

{ #category : #testing }
RxsPiece >> isPlus [

	^min = 1 and: [max == nil]
]

{ #category : #testing }
RxsPiece >> isSingular [
	"A piece with a range is 1 to 1 needs can be compiled
	as a simple match."

	^min = 1 and: [max = 1]
]

{ #category : #testing }
RxsPiece >> isStar [

	^min = 0 and: [max == nil]
]

{ #category : #accessing }
RxsPiece >> max [
	"The value answered may be nil, indicating infinity."

	^max
]

{ #category : #accessing }
RxsPiece >> min [

	^min
]
