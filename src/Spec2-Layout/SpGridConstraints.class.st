"
I am the constraints in which an element is placed into a grid layout.
I have a position and a span.
"
Class {
	#name : #SpGridConstraints,
	#superclass : #SpLayoutConstraints,
	#instVars : [
		'position',
		'span',
		'vAlign',
		'hAlign'
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
SpGridConstraints >> beHorizontalAlignmentCenter [

	self hAlign: SpGridAlignment center
]

{ #category : #accessing }
SpGridConstraints >> beHorizontalAlignmentEnd [

	self hAlign: SpGridAlignment end
]

{ #category : #accessing }
SpGridConstraints >> beHorizontalAlignmentStart [

	self hAlign: SpGridAlignment start
]

{ #category : #accessing }
SpGridConstraints >> beVerticalAlignmentCenter [

	self vAlign: SpGridAlignment center
]

{ #category : #accessing }
SpGridConstraints >> beVerticalAlignmentEnd [

	self vAlign: SpGridAlignment end
]

{ #category : #accessing }
SpGridConstraints >> beVerticalAlignmentStart [

	self vAlign: SpGridAlignment start
]

{ #category : #accessing }
SpGridConstraints >> column [

	^ self position x
]

{ #category : #accessing }
SpGridConstraints >> columnSpan [

	^ self span x
]

{ #category : #private }
SpGridConstraints >> hAlign [
	^ hAlign
]

{ #category : #private }
SpGridConstraints >> hAlign: anObject [
	hAlign := anObject
]

{ #category : #accessing }
SpGridConstraints >> horizontalAlignment [

	^ hAlign
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
SpGridConstraints >> position: anObject [
	position := anObject
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

{ #category : #private }
SpGridConstraints >> vAlign [
	^ vAlign
]

{ #category : #private }
SpGridConstraints >> vAlign: anObject [
	vAlign := anObject
]

{ #category : #accessing }
SpGridConstraints >> verticalAlignment [

	^ vAlign
]
