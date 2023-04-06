"
I'm a IdentitySet with weak elements
"
Class {
	#name : #WeakIdentitySet,
	#superclass : #IdentitySet,
	#instVars : [
		'flag'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #adding }
WeakIdentitySet >> add: newObject [
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
WeakIdentitySet >> noCheckAdd: newObject [
	"This method should be deprecated"

	array
		at: (self findElementOrNil: newObject)
		put: (WeakSetEncloserElement new
				 enclosedElement: newObject;
				 container: self;
				 yourself).
	tally := tally + 1
]
