"
The same as a Set, except that items are compared using `#==` instead of `#=`.

Almost any class named IdentityFoo is the same as Foo except for the way items are compared.  
In Foo, `#=` is used, while in IdentityFoo, `#==` is used.  That is, identity collections will 
treat items as the same only if they have the same identity.

For example, note that copies of a string are equal:
```
	('abc' copy) = ('abc' copy)
```
but they are not identitcal:
```
	('abc' copy) == ('abc' copy)
```
A regular Set will only include equal objects once:
```
	| aSet |
	aSet := Set new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet
```

An IdentitySet will include multiple equal objects if they are not identical:
```
	| aSet |
	aSet := IdentitySet new.
	aSet add: 'abc' copy.
	aSet add: 'abc' copy.
	aSet
```
"
Class {
	#name : #IdentitySet,
	#superclass : #Set,
	#category : #'Collections-Unordered-Sets'
}

{ #category : #converting }
IdentitySet >> asIdentitySet [
	^self
]

{ #category : #testing }
IdentitySet >> identityIncludes: anObject [
	"Don't use a loop, #includes: already uses identity"
	^ self includes: anObject
]

{ #category : #private }
IdentitySet >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start hash |
	hash := anObject identityHash.
	index := start := hash \\ array size + 1.
	[
		| element |
		((element := array at: index) == nil or: [ element enclosedElement == anObject ])
			ifTrue: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]

{ #category : #private }
IdentitySet >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := aKey identityHash \\ array size + 1.
	[
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]
