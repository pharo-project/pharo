"
i am delivered when user scrolling the mouse wheel
"
Class {
	#name : #OSMouseWheelEvent,
	#superclass : #OSMouseEvent,
	#instVars : [
		'scrollHorizontal',
		'scrollVertical'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSMouseWheelEvent >> accept: aVisitor [
	^ aVisitor visitMouseWheelEvent: self
]

{ #category : #accessing }
OSMouseWheelEvent >> scrollHorizontal [
	
	^ scrollHorizontal
]

{ #category : #accessing }
OSMouseWheelEvent >> scrollHorizontal: anObject [
	
	scrollHorizontal := anObject
]

{ #category : #accessing }
OSMouseWheelEvent >> scrollVertical [
	
	^ scrollVertical
]

{ #category : #accessing }
OSMouseWheelEvent >> scrollVertical: anObject [
	
	scrollVertical := anObject
]
