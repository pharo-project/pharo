"
A testing model for testing of the class ComposablePresenterWithModel
"
Class {
	#name : #TestingPointModel,
	#superclass : #Model,
	#instVars : [
		'x',
		'y'
	],
	#category : #Spec-Tests-Utils
}

{ #category : #'instance creation' }
TestingPointModel class >> x: xInteger y: yInteger [ 

	^ self new setX: xInteger setY: yInteger
]

{ #category : #accessing }
TestingPointModel >> setX: xValue setY: yValue [ 

	x := xValue.
	y := yValue
]

{ #category : #accessing }
TestingPointModel >> x [
	^ x
]

{ #category : #accessing }
TestingPointModel >> x: anObject [
	x := anObject
]

{ #category : #accessing }
TestingPointModel >> y [
	^ y
]

{ #category : #accessing }
TestingPointModel >> y: anObject [
	y := anObject
]
