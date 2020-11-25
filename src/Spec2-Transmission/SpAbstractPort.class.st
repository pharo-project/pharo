"
I'm a base port.
"
Class {
	#name : #SpAbstractPort,
	#superclass : #Object,
	#instVars : [
		'name',
		'presenter',
		'delegation'
	],
	#category : #'Spec2-Transmission-Base'
}

{ #category : #testing }
SpAbstractPort class >> isAbstract [ 
	^self name = #SpAbstractPort
]

{ #category : #'instance creation' }
SpAbstractPort class >> newDelegate: aBlock [ 

	^ self new 
		delegateTo: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpAbstractPort class >> newName: aName [

	^ self new 
		name: aName;
		yourself
]

{ #category : #'instance creation' }
SpAbstractPort class >> newName: aName delegate: aBlock [ 

	^ self new 
		name: aName;
		delegateTo: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpAbstractPort class >> newPresenter: aPresenter [

	^ self new 
		presenter: aPresenter;
		yourself
]

{ #category : #accessing }
SpAbstractPort class >> portName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpAbstractPort >> delegateTo: aPresenter [

	delegation := aPresenter
]

{ #category : #accessing }
SpAbstractPort >> destinationPresenter [

	^ delegation 
		ifNotNil: [ delegation value ]
		ifNil: [ presenter ]
]

{ #category : #testing }
SpAbstractPort >> isInput [ 

	^ false
]

{ #category : #testing }
SpAbstractPort >> isOutput [

	^ false
]

{ #category : #accessing }
SpAbstractPort >> name [

	^ name ifNil: [ self class portName ]
]

{ #category : #accessing }
SpAbstractPort >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpAbstractPort >> presenter [ 

	^ presenter
]

{ #category : #accessing }
SpAbstractPort >> presenter: aPresenter [

	presenter := aPresenter
]
