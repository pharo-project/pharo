"
I am an internal popup brick event listener. I know a popup I belong to
"
Class {
	#name : #GLMPopupBrickListener,
	#superclass : #Object,
	#instVars : [
		'popup',
		'predicates'
	],
	#category : 'Glamour-Morphic-Brick-Widgets-UI-Popup'
}

{ #category : #'instance creation' }
GLMPopupBrickListener class >> predicate: aPopupPredicate [

	^ self new
		predicate: aPopupPredicate;
		yourself
]

{ #category : #action }
GLMPopupBrickListener >> act [
	"Perform an action when predicate is activated"
	self subclassResponsibility
]

{ #category : #events-processing }
GLMPopupBrickListener >> handleListenEvent: anEvent [

	self predicates
		detect: [ :aPredicate | aPredicate popup: self popup event: anEvent ]
		ifFound: [ :aPredicate | self act ]
]

{ #category : #initialization }
GLMPopupBrickListener >> initialize [
	super initialize.
	
	predicates := OrderedCollection new
]

{ #category : #accessing }
GLMPopupBrickListener >> popup [
	^ popup
]

{ #category : #accessing }
GLMPopupBrickListener >> popup: anObject [
	popup := anObject
]

{ #category : #accessing }
GLMPopupBrickListener >> predicate: anObject [
	predicates add: anObject
]

{ #category : #accessing }
GLMPopupBrickListener >> predicates [
	^ predicates
]
