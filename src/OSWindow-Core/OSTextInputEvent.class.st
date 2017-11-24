"
i am delivered when user enters some text
"
Class {
	#name : #OSTextInputEvent,
	#superclass : #OSEvent,
	#instVars : [
		'text',
		'modifiers',
		'position'
	],
	#category : #'OSWindow-Core-Events'
}

{ #category : #visitor }
OSTextInputEvent >> accept: aVisitor [
	^ aVisitor visitTextInputEvent: self
]

{ #category : #initialization }
OSTextInputEvent >> initialize [
	super initialize.
	modifiers := OSStateModifiers new.
]

{ #category : #accessing }
OSTextInputEvent >> modifiers [
	
	^ modifiers
]

{ #category : #accessing }
OSTextInputEvent >> modifiers: anObject [
	
	modifiers := anObject
]

{ #category : #accessing }
OSTextInputEvent >> position [
	
	^ position
]

{ #category : #accessing }
OSTextInputEvent >> position: anObject [
	
	position := anObject
]

{ #category : #accessing }
OSTextInputEvent >> text [
	
	^ text
]

{ #category : #accessing }
OSTextInputEvent >> text: anObject [
	
	text := anObject
]
