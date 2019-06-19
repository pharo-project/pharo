"
I implement first class function to be used instead of block in sorted collection.
There is system SortFunction. I am introduced to be safely used in caches.
Now in Pharo 7 SortFunction is improved and can completally replace and can be used instead of me.
But for Pharo 6 compatibility am still here and use by other Calypso based packages.

So for general overview read system SortFunction comments.

Internal Representation and Key Implementation Points.

    Instance Variables
	direction:		<Integer>
"
Class {
	#name : #ClySortFunction,
	#superclass : #Object,
	#instVars : [
		'direction'
	],
	#category : #'Calypso-NavigationModel-Model'
}

{ #category : #'instance creation' }
ClySortFunction class >> ascending [

	^self new
		direction: 1
]

{ #category : #'instance creation' }
ClySortFunction class >> descending [

	^self new 
		direction: -1
]

{ #category : #comparing }
ClySortFunction >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ direction = anObject direction
]

{ #category : #evaluating }
ClySortFunction >> collate: anObject1 with: anObject2 [
	self subclassResponsibility 
]

{ #category : #accessing }
ClySortFunction >> direction [
	^ direction
]

{ #category : #accessing }
ClySortFunction >> direction: anObject [
	direction := anObject
]

{ #category : #converting }
ClySortFunction >> forBrowserItems [
	^ClySortBrowserItemFunction with: self
]

{ #category : #comparing }
ClySortFunction >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ direction hash
]

{ #category : #initialization }
ClySortFunction >> initialize [
	super initialize.
	direction := 1
]

{ #category : #evaluating }
ClySortFunction >> value: anObject1 value: anObject2 [
	| result |
	result := (self collate: anObject1 with: anObject2) * direction.
	^result <= 0
]
