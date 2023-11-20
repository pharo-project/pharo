Extension { #name : 'Collection' }

{ #category : '*Collections-Unordered' }
Collection >> asBag [

	"Answer a Bag whose elements are the elements of the receiver."

	"{1. 2} asBag =    {2. 1} asBag >>> true"
	"{1. 2} asBag = {1. 2. 2} asBag >>> false"
	"{1. 2} asBag = {1. 2. 3} asBag >>> false"
	"#() asBag = Bag new >>> true"

	^ Bag withAll: self
]

{ #category : '*Collections-Unordered' }
Collection >> asDictionary [

	"Answers a Dictionary based on collection of Associations."

	"{'one' -> 1. 'two' ->2} asDictionary >>> (Dictionary with: 'one' -> 1 with: 'two' ->2)"
	"{'two' ->2. 'one' -> 1} asDictionary >>> (Dictionary with: 'one' -> 1 with: 'two' ->2)"
	"#() asDictionary >>> Dictionary new"

	^ self as: Dictionary
]

{ #category : '*Collections-Unordered' }
Collection >> asIdentitySet [

	"Return a new IdentitySet based on self."

	"{ 2. 1+1 } asIdentitySet size >>> 1"
	"{ 'foo'. ('fo','o') } asIdentitySet size >>> 2"

	^(IdentitySet new: self size) addAll: self; yourself
]

{ #category : '*Collections-Unordered' }
Collection >> asSet [
	"Answer a Set whose elements are the unique elements of the receiver."

	"{1. 2} asSet =    {2. 1} asSet >>> true"
	"{1. 2} asSet = {1. 2. 2} asSet >>> true"
	"{1. 2} asSet = {1. 2. 3} asSet >>> false"
	"{} asSet = Set new >>> true"

	^ Set withAll: self
]

{ #category : '*Collections-Unordered' }
Collection >> flatCollectAsSet: aBlock [
	"Evaluate aBlock for each of the receiver's elements and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"

	"( #(1 2 3) flatCollectAsSet: [:each | { each. each+1 } ] ) >>> #(1 2 3 4) asSet"

	^ self flatCollect: aBlock as: Set
]
