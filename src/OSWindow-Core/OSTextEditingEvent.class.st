"
I am an event that happens when editing text that is not yet committed.
Often happens when the user invoces an input method editor (IME) to type in languages like japanese or chinese, or when one holds a key and accents are proposed.
"
Class {
	#name : 'OSTextEditingEvent',
	#superclass : 'OSTextInputEvent',
	#instVars : [
		'start',
		'length'
	],
	#category : 'OSWindow-Core-Events',
	#package : 'OSWindow-Core',
	#tag : 'Events'
}

{ #category : 'visitor' }
OSTextEditingEvent >> accept: aVisitor [
	^ aVisitor visitTextEditingEvent: self
]

{ #category : 'accessing' }
OSTextEditingEvent >> length [

	^ length
]

{ #category : 'accessing' }
OSTextEditingEvent >> length: anObject [

	length := anObject
]

{ #category : 'accessing' }
OSTextEditingEvent >> start [

	^ start
]

{ #category : 'accessing' }
OSTextEditingEvent >> start: anObject [

	start := anObject
]
