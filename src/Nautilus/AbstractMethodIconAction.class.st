"
AbstractMethodIconAction is the common super class for all the method icon actions.


A method icon action is used to retrieve the correct icon the method in nautilus lists
"
Class {
	#name : #AbstractMethodIconAction,
	#superclass : #Object,
	#instVars : [
		'method',
		'browser',
		'icon'
	],
	#category : #'Nautilus-Widgets'
}

{ #category : #'instance creation' }
AbstractMethodIconAction class >> for: aMethod in: aBrowser [

	^ self new
		method: aMethod;
		browser: aBrowser;
		yourself
]

{ #category : #order }
AbstractMethodIconAction >> actionIcon [
	"Return the icon for this action"
	
	^ icon := self privateActionIcon
]

{ #category : #order }
AbstractMethodIconAction >> actionOrder [
	"Return the priority of this action"
	
	^ self subclassResponsibility
]

{ #category : #order }
AbstractMethodIconAction >> actionStateToCache [
	"Return the state of the icon for caching purpose"
	
	^ IconicButtonStateHolder forNautilus: icon
]

{ #category : #accessing }
AbstractMethodIconAction >> browser [

	^ browser
]

{ #category : #accessing }
AbstractMethodIconAction >> browser: aBrowser [

	browser := aBrowser
]

{ #category : #order }
AbstractMethodIconAction >> iconExtent [

	^16@16
]

{ #category : #accessing }
AbstractMethodIconAction >> iconForTestResultOf: aMethod [
	aMethod isPassedTest
		ifTrue: [ ^ self iconNamed: #testGreenIcon ].
	aMethod isFailedTest
		ifTrue: [ ^ self iconNamed: #testYellowIcon ].
	aMethod isErrorTest
		ifTrue: [ ^ self iconNamed: #testRedIcon ].
	^ self iconNamed: #testNotRunIcon.
]

{ #category : #order }
AbstractMethodIconAction >> isActionHandled [
	"Return true if the provided method fits this action requirement. Implementation note: The best way to make sure that the system will break when we will add a new action is to use self subclassResponsibility for this method. So by default return false to avoid blowing up the system under your fingers!"
	^ false
]

{ #category : #accessing }
AbstractMethodIconAction >> method [

	^ method
]

{ #category : #accessing }
AbstractMethodIconAction >> method: aMethod [

	method := aMethod
]

{ #category : #order }
AbstractMethodIconAction >> privateActionIcon [
	
	^ self subclassResponsibility
]
