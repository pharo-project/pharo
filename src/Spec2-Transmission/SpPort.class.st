"
I'm a base port.
"
Class {
	#name : #SpPort,
	#superclass : #Object,
	#instVars : [
		'name',
		'presenter',
		'delegation'
	],
	#category : #'Spec2-Transmission-Base'
}

{ #category : #'instance creation' }
SpPort class >> newDelegate: aBlock [ 

	^ self new 
		delegateTo: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpPort class >> newName: aName [

	^ self new 
		name: aName;
		yourself
]

{ #category : #'instance creation' }
SpPort class >> newName: aName delegate: aBlock [ 

	^ self new 
		name: aName;
		delegateTo: aBlock;
		yourself
]

{ #category : #accessing }
SpPort class >> portName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpPort >> delegateTo: aPresenter [

	delegation := aPresenter
]

{ #category : #accessing }
SpPort >> destinationPresenter [

	^ delegation 
		ifNotNil: [ delegation value ]
		ifNil: [ presenter ]
]

{ #category : #testing }
SpPort >> isInput [ 

	^ false
]

{ #category : #testing }
SpPort >> isOutput [

	^ false
]

{ #category : #accessing }
SpPort >> name [

	^ name ifNil: [ self class portName ]
]

{ #category : #accessing }
SpPort >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpPort >> presenter [ 

	^ presenter
]

{ #category : #accessing }
SpPort >> presenter: aPresenter [

	presenter := aPresenter
]
