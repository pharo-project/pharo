"
I'm a Set with weak elements
"
Class {
	#name : #WeakSet,
	#superclass : #Set,
	#instVars : [
		'flag'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #adding }
WeakSet >> add: newObject [
	"Include newObject as one of the receiver's elements, but only if
	not already present. Answer newObject."

	| index |
	index := self scanFor: newObject.
	(array at: index) ifNil: [
		self atNewIndex: index put: (WeakSetEncloserElement new
				 enclosedElement: newObject;
				 container: self;
				 yourself) ].
	^ newObject
]

{ #category : #private }
WeakSet >> noCheckAdd: newObject [
	"This method should be deprecated"

	array
		at: (self findElementOrNil: newObject)
		put: (WeakSetEncloserElement new
				 enclosedElement: newObject;
				 container: self;
				 yourself).
	tally := tally + 1
]
