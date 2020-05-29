"
I am the constraints in which an element is placed into a grid layout.
I have a position and a span.
"
Class {
	#name : #SpGridConstraints,
	#superclass : #SpLayoutConstraints,
	#traits : 'SpTAlignable',
	#classTraits : 'SpTAlignable classTrait',
	#instVars : [
		'position',
		'span'
	],
	#category : #'Spec2-Layout-Grid'
}

{ #category : #'instance creation' }
SpGridConstraints class >> at: aPoint [

	^ self new 
		position: aPoint;
		yourself
]

{ #category : #'instance creation' }
SpGridConstraints class >> at: aPoint span: spanPoint [

	^ self new 
		position: aPoint;
		span: spanPoint;
		yourself
]

{ #category : #accessing }
SpGridConstraints >> column [

	^ self position x
]

{ #category : #accessing }
SpGridConstraints >> columnSpan [

	^ self span x
]

{ #category : #initialization }
SpGridConstraints >> initialize [

	super initialize.
	self position: 1@1.
	self span: 1@1
]

{ #category : #accessing }
SpGridConstraints >> position [

	^ position
]

{ #category : #accessing }
SpGridConstraints >> position: aPoint [

	position := aPoint
]

{ #category : #accessing }
SpGridConstraints >> row [

	^ self position y
]

{ #category : #accessing }
SpGridConstraints >> rowSpan [

	^ self span y
]

{ #category : #accessing }
SpGridConstraints >> span [
	^ span
]

{ #category : #accessing }
SpGridConstraints >> span: anObject [
	span := anObject
]
