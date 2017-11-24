"
I am a variable node for representing a  temporary  variable stored in a  Context object that has a name attached. This includes  local variables and method parameters.
"
Class {
	#name : #GTInspectorContextNamedTempNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'tempName',
		'tempIndex',
		'tag'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorContextNamedTempNode class >> hostObject: aContext tempName: aString tempIndex: anInteger [
	^ (self hostObject: aContext) 
		tempName: aString;
		tempIndex: anInteger;
		variableTag: ((aContext method argumentNames includes: aString)
			ifTrue: [ 'parameter' ]
			ifFalse: [ 'temp' ])
		yourself
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> errorMessage [
	^ 'error obtaining field value'
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> key [
	^ self tempName
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> label [
	^ '[' , self key , ']'
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> rawValue [
	^ self hostObject debuggerMap namedTempAt: tempIndex in: self hostObject
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> tempIndex [
	^ tempIndex
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> tempIndex: anObject [
	tempIndex := anObject
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> tempName [
	^ tempName
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> tempName: aString [
	tempName := aString
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> variableTag [
	^ tag ifNil: [ 'temp' ]
]

{ #category : #accessing }
GTInspectorContextNamedTempNode >> variableTag: aTag [
	tag := aTag
]
