Class {
	#name : #SpecGridConstraints,
	#superclass : #SpecLayoutConstraints,
	#instVars : [
		'position',
		'span'
	],
	#category : #'Spec-Layout-Grid'
}

{ #category : #'instance creation' }
SpecGridConstraints class >> at: aPoint [

	^ self new 
		position: aPoint;
		yourself
]

{ #category : #'instance creation' }
SpecGridConstraints class >> at: aPoint span: spanPoint [

	^ self new 
		position: aPoint;
		span: spanPoint;
		yourself
]

{ #category : #accessing }
SpecGridConstraints >> column [

	^ self position x
]

{ #category : #accessing }
SpecGridConstraints >> columnSpan [

	^ self span x
]

{ #category : #initialization }
SpecGridConstraints >> initialize [

	super initialize.
	self position: 1@1.
	self span: 1@1
]

{ #category : #accessing }
SpecGridConstraints >> position [
	^ position
]

{ #category : #accessing }
SpecGridConstraints >> position: anObject [
	position := anObject
]

{ #category : #accessing }
SpecGridConstraints >> row [

	^ self position y
]

{ #category : #accessing }
SpecGridConstraints >> rowSpan [

	^ self span y
]

{ #category : #accessing }
SpecGridConstraints >> span [
	^ span
]

{ #category : #accessing }
SpecGridConstraints >> span: anObject [
	span := anObject
]
