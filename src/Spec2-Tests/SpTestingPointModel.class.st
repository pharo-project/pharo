"
A testing model for testing of the class ComposablePresenterWithModel
"
Class {
	#name : #SpTestingPointModel,
	#superclass : #Model,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Spec2-Tests-Utils'
}

{ #category : #'instance creation' }
SpTestingPointModel class >> x: xInteger y: yInteger [ 

	^ self new setX: xInteger setY: yInteger
]

{ #category : #accessing }
SpTestingPointModel >> setX: xValue setY: yValue [ 

	x := xValue.
	y := yValue
]

{ #category : #accessing }
SpTestingPointModel >> x [
	^ x
]

{ #category : #accessing }
SpTestingPointModel >> x: anObject [
	x := anObject
]

{ #category : #accessing }
SpTestingPointModel >> y [
	^ y
]

{ #category : #accessing }
SpTestingPointModel >> y: anObject [
	y := anObject
]
