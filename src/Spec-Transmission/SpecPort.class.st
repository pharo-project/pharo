"
I'm a base port.
"
Class {
	#name : #SpecPort,
	#superclass : #Object,
	#instVars : [
		'name',
		'presenter',
		'delegation'
	],
	#category : #'Spec-Transmission-Base'
}

{ #category : #'instance creation' }
SpecPort class >> newDelegate: aBlock [ 

	^ self new 
		delegateTo: aBlock;
		yourself
]

{ #category : #'instance creation' }
SpecPort class >> newName: aName [

	^ self new 
		name: aName;
		yourself
]

{ #category : #'instance creation' }
SpecPort class >> newName: aName delegate: aBlock [ 

	^ self new 
		name: aName;
		delegateTo: aBlock;
		yourself
]

{ #category : #accessing }
SpecPort class >> portName [

	^ self subclassResponsibility
]

{ #category : #accessing }
SpecPort >> delegateTo: aPresenter [

	delegation := aPresenter
]

{ #category : #accessing }
SpecPort >> destinationPresenter [

	^ delegation 
		ifNotNil: [ delegation value ]
		ifNil: [ presenter ]
]

{ #category : #testing }
SpecPort >> isInput [ 

	^ false
]

{ #category : #testing }
SpecPort >> isOutput [

	^ false
]

{ #category : #accessing }
SpecPort >> name [

	^ name ifNil: [ self class portName ]
]

{ #category : #accessing }
SpecPort >> name: anObject [
	name := anObject
]

{ #category : #accessing }
SpecPort >> presenter [ 

	^ presenter
]

{ #category : #accessing }
SpecPort >> presenter: aPresenter [

	presenter := aPresenter
]
