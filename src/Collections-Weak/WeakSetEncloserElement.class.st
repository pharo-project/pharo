"
I am a helper class for handling the contents of weak set collection.
My main purpose is to keep my values ""weakly"" and remove the corresponding collection entry when the element is garbage collected.

I use ephemerons to get notified of the collection of my enclosed element.
"
Class {
	#name : #WeakSetEncloserElement,
	#superclass : #Object,
	#type : #ephemeron,
	#instVars : [
		'enclosedElement',
		'container'
	],
	#category : #'Collections-Weak-Base'
}

{ #category : #accessing }
WeakSetEncloserElement >> container [

	^ container
]

{ #category : #accessing }
WeakSetEncloserElement >> container: anObject [

	container := anObject
]

{ #category : #accessing }
WeakSetEncloserElement >> enclosedElement [

	"To be used inside of weak sets"
	^ enclosedElement
]

{ #category : #accessing }
WeakSetEncloserElement >> enclosedElement: anObject [

	enclosedElement := anObject
]

{ #category : #finalization }
WeakSetEncloserElement >> mourn [

	container remove: enclosedElement ifAbsent: [
		"Do Nothing. Probably somebody removed the key while I was in the mourn queue" ]
]
