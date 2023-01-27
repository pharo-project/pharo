"
Like a Dictionary, except that keys are compared with #== instead of #= .

See the comment of IdentitySet for more information.
"
Class {
	#name : #IdentityDictionary,
	#superclass : #Dictionary,
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #private }
IdentityDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock."

	self associationsDo:
		[:association | value == association value ifTrue: [^ association key]].
	^ exceptionBlock value
]

{ #category : #private }
IdentityDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish start element |
	finish := array size.
	start := (anObject identityHash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #private }
IdentityDictionary >> scanForEmptySlotFor: aKey [
	"Scan the key array for the first slot containing an empty slot (indicated by a nil). Answer the index of that slot. This method will be overridden in various subclasses that have different interpretations for matching elements."

	| index start |
	index := start := aKey identityHash \\ array size + 1.
	[
		(array at: index) ifNil: [ ^index ].
		(index := index \\ array size + 1) = start ] whileFalse.
	self errorNoFreeSpace
]
