"
This is an example for a virtual slot that computes its value. 

It stores a block which is evaluated with the object as a parameter to calculate the value
on read. Writing is ignored.

e.g. make a class lile this:

Object subclass: #TT
	slots: { #i => ComputedSlot with: [ :o | o class methods size ] }
	classVariables: {  }
	category: 'TT'
"
Class {
	#name : #ComputedSlot,
	#superclass : #Slot,
	#instVars : [
		'block'
	],
	#category : #Slot-Examples
}

{ #category : #comparing }
ComputedSlot >> = other [
	^ super = other and: [block = other block]
]

{ #category : #accessing }
ComputedSlot >> block [
	^block
]

{ #category : #comparing }
ComputedSlot >> hash [
	^super hash bitXor: block hash
]

{ #category : #printing }
ComputedSlot >> printOn: aStream [
	aStream 
		store: self name;
		nextPutAll: ' => ';
		nextPutAll: self class name;
		nextPutAll: ' with: ';
		nextPutAll: block printString
]

{ #category : #meta-object-protocol }
ComputedSlot >> read: anObject [
	^block value: anObject
	
]

{ #category : #'instance creation' }
ComputedSlot >> with: aBlock [
	block := aBlock
]

{ #category : #meta-object-protocol }
ComputedSlot >> write: aValue to: anObject [
	"ignored"
]
