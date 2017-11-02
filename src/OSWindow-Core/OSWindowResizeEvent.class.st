"
I am delivered when window resized by user or OS/window manager.
"
Class {
	#name : #OSWindowResizeEvent,
	#superclass : #OSWindowEvent,
	#instVars : [
		'width',
		'height'
	],
	#category : 'OSWindow-Core-Events'
}

{ #category : #visitor }
OSWindowResizeEvent >> accept: aVisitor [
	^ aVisitor visitWindowResizeEvent: self
]

{ #category : #visitor }
OSWindowResizeEvent >> defaultAction [
	window renderer ifNotNil: [ :renderer | renderer resized ]
]

{ #category : #accessing }
OSWindowResizeEvent >> height [
	^ height
]

{ #category : #accessing }
OSWindowResizeEvent >> height: anInteger [ 
	height := anInteger.
]

{ #category : #accessing }
OSWindowResizeEvent >> width [
	^ width
]

{ #category : #accessing }
OSWindowResizeEvent >> width: anInteger [ 
	width := anInteger.
]
