"
I am a variable node for representing any dynamically computed attribute that should be displayed in the Raw presentation. I should not be used for slots or indexed variables.
"
Class {
	#name : #GTInspectorDynamicNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'label',
		'value',
		'tag'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorDynamicNode class >> hostObject: anObject label: aString value: anotherObject [
	^ (self hostObject: anObject) 
		label: aString;
		rawValue: anotherObject;
		yourself
]

{ #category : #accessing }
GTInspectorDynamicNode >> key [
	^ label
]

{ #category : #accessing }
GTInspectorDynamicNode >> label [
	^ label
]

{ #category : #accessing }
GTInspectorDynamicNode >> label: aString [
	label := aString
]

{ #category : #accessing }
GTInspectorDynamicNode >> rawValue [
	^ value
]

{ #category : #accessing }
GTInspectorDynamicNode >> rawValue: anObject [
	value := anObject
]

{ #category : #accessing }
GTInspectorDynamicNode >> variableTag [
	^  tag ifNil: [ super variableTag ]
]

{ #category : #accessing }
GTInspectorDynamicNode >> variableTag: aTag [
	tag := aTag
]
