"
Class PluggableDictionary allows the redefinition of hashing and equality by clients. This is in particular useful if the clients know about specific properties of the objects stored in the dictionary. See the class comment of PluggableSet for an example.

Instance variables:
	hashBlock	<BlockClosure>	A one-argument block used for hashing the keys.
	equalBlock	<BlockClosure>	A two-argument block used for comparing the keys.

"
Class {
	#name : #PluggableDictionary,
	#superclass : #Dictionary,
	#instVars : [
		'hashBlock',
		'equalBlock'
	],
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #accessing }
PluggableDictionary class >> integerDictionary [
	^ self new hashBlock: [:integer | integer hash \\ 1064164 * 1009]
]

{ #category : #comparing }
PluggableDictionary >> = aDictionary [
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value.
	See issue 16760 before changing"

	self == aDictionary ifTrue: [^true].
	self species == aDictionary species ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self equalBlock = aDictionary equalBlock ifFalse: [^false].
	self hashBlock = aDictionary hashBlock ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true
]

{ #category : #copying }
PluggableDictionary >> copyEmpty [
	^super copyEmpty
		hashBlock: hashBlock;
		equalBlock: equalBlock
]

{ #category : #accessing }
PluggableDictionary >> equalBlock [
	"Return the block used for comparing the elements in the receiver."
	^equalBlock
]

{ #category : #accessing }
PluggableDictionary >> equalBlock: aBlock [
	"Set a new equality block. The block must accept two arguments and return true if the argumets are considered to be equal, false otherwise"
	equalBlock := aBlock
]

{ #category : #comparing }
PluggableDictionary >> hash [
	"hashBlock is used to hash keys for lookup, not the dictionary itself, but its hash is still
	considered."
	^ (super hash bitXor: self equalBlock hash) bitXor: self hashBlock hash
]

{ #category : #accessing }
PluggableDictionary >> hashBlock [
	"Return the block used for hashing the keys in the receiver."
	^hashBlock
]

{ #category : #accessing }
PluggableDictionary >> hashBlock: aBlock [
	"Set a new hash block. The block must accept one argument and must return the hash value of the given argument."
	hashBlock := aBlock
]

{ #category : #private }
PluggableDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil
(indicating
	  an empty slot) or an element that matches anObject. Answer the index

	of that slot or zero if no slot is found. This  method will be
overridden
	in various subclasses that have different interpretations for matching

	elements."
	| element start finish |
	start := (hashBlock ifNil: [anObject hash]
				ifNotNil: [hashBlock value: anObject])
				\\ array size + 1.
	finish := array size.
	"Search from (hash mod size) to the end."
	start to: finish do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element key = anObject]
				ifNotNil: [equalBlock value: element key value: anObject]])
			ifTrue: [^ index]].
	"Search from 1 to where we started."
	1 to: start - 1 do: [:index | ((element := array at: index) == nil or:
[equalBlock ifNil: [element key = anObject]
				ifNotNil: [equalBlock value: element key value: anObject]])
			ifTrue: [^ index]].
	^ 0"No match AND no empty slot"
]

{ #category : #private }
PluggableDictionary >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := (hashBlock
		ifNil: [ aKey hash ]
		ifNotNil: [ hashBlock value: aKey ]) \\ array size + 1.
	[
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]
