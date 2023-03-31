Class {
	#name : #WeakKeyAssociation2,
	#superclass : #Association,
	#type : #ephemeron,
	#instVars : [
		'container'
	],
	#category : #'Collections-Weak-Dictionaries'
}

{ #category : #accessing }
WeakKeyAssociation2 >> container [

	^ container
]

{ #category : #accessing }
WeakKeyAssociation2 >> container: anObject [

	container := anObject
]

{ #category : #accessing }
WeakKeyAssociation2 >> expire [
    "Release the value if the key has gone. Return true if a modification was made."
	(self basicAt: 1) ifNil:[
		value == self ifTrue:[^false].
		value := self.
		^true
	].
	^false
]

{ #category : #accessing }
WeakKeyAssociation2 >> expired [
	^value == self
]

{ #category : #finalization }
WeakKeyAssociation2 >> mourn [

	container removeKey: key
]
