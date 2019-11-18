"
SUnit tests for pluggable dictionaries
"
Class {
	#name : #PluggableDictionaryTest,
	#superclass : #DictionaryTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #coverage }
PluggableDictionaryTest class >> classToBeTested [

	^ IdentitySet
]

{ #category : #'building suites' }
PluggableDictionaryTest class >> shouldInheritSelectors [

^true
]

{ #category : #coverage }
PluggableDictionaryTest >> classToBeTested [

	^ PluggableDictionary
]

{ #category : #'tests - copy - clone' }
PluggableDictionaryTest >> testCopyEmpty [
	| pluggableDict copiedPluggableDict |
	super testCopyEmpty.

	(pluggableDict := self emptyDict)
		equalBlock: [ :a :b | a = b ];
		hashBlock: [ :a | a hash ].

	copiedPluggableDict := pluggableDict copyEmpty.
	self assert: copiedPluggableDict equalBlock identicalTo: pluggableDict equalBlock.
	self assert: copiedPluggableDict hashBlock identicalTo: pluggableDict hashBlock
]

{ #category : #'tests - comparing' }
PluggableDictionaryTest >> testEqualBlock [
	| pluggableDict assoc |
	pluggableDict := self emptyDict.
	self assert: pluggableDict equalBlock isNil.
	self assert: (pluggableDict equalBlock: [ :a :b | a = b ]) identicalTo: pluggableDict.

	assoc := 'newKey' -> 'newValue'.
	pluggableDict add: assoc.
	self assert: (pluggableDict includesAssociation: assoc).

	pluggableDict equalBlock: [ :a :b | a == b ].
	self assert: (pluggableDict includesAssociation: assoc).
	pluggableDict
		removeKey: assoc key;
		add: assoc key copy -> assoc value.
	self deny: (pluggableDict includesAssociation: assoc).

	pluggableDict equalBlock: [ :a :b | a = b ].
	self assert: (pluggableDict includesAssociation: assoc)
]

{ #category : #'tests - comparing' }
PluggableDictionaryTest >> testEqualBlockEquality [
	| pluggableDict pluggableDict2 equalBlock |
	pluggableDict := self nonEmptyDict.
	pluggableDict2 := self nonEmptyDict copy.

	equalBlock := [ :a :b | a = b ].
	pluggableDict2 equalBlock: equalBlock.
	self deny: pluggableDict equals: pluggableDict2.
	self deny: pluggableDict2 equals: pluggableDict.

	pluggableDict equalBlock: equalBlock.
	self assert: pluggableDict equals: pluggableDict2.
	self assert: pluggableDict2 equals: pluggableDict
]

{ #category : #'tests - comparing' }
PluggableDictionaryTest >> testHashBlock [
	| pluggableDict keys |
	pluggableDict := self nonEmptyDict.
	keys := pluggableDict keys.
	self assert: pluggableDict hashBlock isNil.
	self assert: (pluggableDict hashBlock: [ :a | a hash ]) identicalTo: pluggableDict.

	keys do: [ :each | self assert: (pluggableDict includesKey: each) ].
	self deny: (pluggableDict includesKey: self keyNotIn)
]

{ #category : #'tests - comparing' }
PluggableDictionaryTest >> testHashBlockEquality [
	| pluggableDict pluggableDict2 hashBlock |
	pluggableDict := self nonEmptyDict.
	pluggableDict2 := self nonEmptyDict copy.

	hashBlock := [ :a | a hash ].
	pluggableDict2 hashBlock: hashBlock.
	self deny: pluggableDict equals: pluggableDict2.
	self deny: pluggableDict2 equals: pluggableDict.

	pluggableDict hashBlock: hashBlock.
	self assert: pluggableDict equals: pluggableDict2.
	self assert: pluggableDict2 equals: pluggableDict
]
