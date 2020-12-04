"
I'm an external property of some entity related to a piece of code.

I define a basic interface of a #title and an #icon that can be used to display it in a user interface
I also have a source anchor with a reference to the actual class, method, or other source code entity that I am related to.
"
Class {
	#name : #ReProperty,
	#superclass : #Model,
	#instVars : [
		'sourceAnchor'
	],
	#category : #'Renraku-Critiques'
}

{ #category : #actions }
ReProperty >> actions [ 

	^ #()
]

{ #category : #accessing }
ReProperty >> color [

	^ Color black alpha: 0.1
]

{ #category : #accessing }
ReProperty >> entity [
	^ self sourceAnchor entity
]

{ #category : #accessing }
ReProperty >> icon [

	^ Form extent: 16@16 depth: 32
]

{ #category : #initialization }
ReProperty >> initializeSourceAnchor: anAnchor [

	self initialize.
	sourceAnchor := anAnchor
]

{ #category : #testing }
ReProperty >> isCritique [
	^ false
]

{ #category : #accessing }
ReProperty >> sourceAnchor [
	^ sourceAnchor
]

{ #category : #accessing }
ReProperty >> title [

	^ self subclassResponsibility 
]
