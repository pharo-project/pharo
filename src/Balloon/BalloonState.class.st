"
This class is a repository for data which needs to be preserved during certain operations of BalloonCanvas.
"
Class {
	#name : #BalloonState,
	#superclass : #Object,
	#instVars : [
		'transform',
		'colorTransform',
		'aaLevel'
	],
	#category : #Balloon-Engine
}

{ #category : #accessing }
BalloonState >> aaLevel [
	^aaLevel
]

{ #category : #accessing }
BalloonState >> aaLevel: aNumber [
	aaLevel := aNumber
]

{ #category : #accessing }
BalloonState >> colorTransform [
	^colorTransform
]

{ #category : #accessing }
BalloonState >> colorTransform: aColorTransform [
	colorTransform := aColorTransform
]

{ #category : #accessing }
BalloonState >> transform [
	^transform
]

{ #category : #accessing }
BalloonState >> transform: aMatrixTransform [
	transform := aMatrixTransform
]
