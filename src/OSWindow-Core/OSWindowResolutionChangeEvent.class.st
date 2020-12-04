"
I am delivered when the per-pixel resolution measured in DPI of a window is changed.
"
Class {
	#name : #OSWindowResolutionChangeEvent,
	#superclass : #OSWindowEvent,
	#instVars : [
		'screenScaleFactor',
		'horizontalDPI',
		'verticalDPI',
		'diagonalDPI'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowResolutionChangeEvent >> accept: visitor [
	^ visitor visitWindowResolutionChangeEvent: self
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> diagonalDPI [
	^ diagonalDPI
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> diagonalDPI: anObject [
	diagonalDPI := anObject
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> horizontalDPI [
	^ horizontalDPI
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> horizontalDPI: anObject [
	horizontalDPI := anObject
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> screenScaleFactor [
	^ screenScaleFactor
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> screenScaleFactor: anObject [
	screenScaleFactor := anObject
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> verticalDPI [
	^ verticalDPI
]

{ #category : #accessing }
OSWindowResolutionChangeEvent >> verticalDPI: anObject [
	verticalDPI := anObject
]
