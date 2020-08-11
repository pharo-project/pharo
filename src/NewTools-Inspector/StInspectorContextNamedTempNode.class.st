"
I am a variable node for representing a temporary variable stored in a Context object that has a name attached. 
This includes  local variables and method parameters.
"
Class {
	#name : #StInspectorContextNamedTempNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'tempName',
		'tempIndex',
		'tag'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StInspectorContextNamedTempNode class >> hostObject: aContext tempName: aString tempIndex: anInteger [
	^ (self hostObject: aContext) 
		tempName: aString;
		tempIndex: anInteger;
		variableTag: ((aContext method argumentNames includes: aString)
			ifTrue: [ 'parameter' ]
			ifFalse: [ 'temp' ])
		yourself
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> errorMessage [
	^ 'error obtaining field value'
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> key [
	^ self tempName
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> label [
	^ self key
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> rawValue [
	^ [ self hostObject tempNamed: tempName ]
		  on: SubscriptOutOfBounds
		  do: [ :err | 'cannot read ' , tempName ]
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> tempIndex [
	^ tempIndex
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> tempIndex: anObject [
	tempIndex := anObject
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> tempName [
	^ tempName
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> tempName: aString [
	tempName := aString
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> variableTag [
	^ tag ifNil: [ 'temp' ]
]

{ #category : #accessing }
StInspectorContextNamedTempNode >> variableTag: aTag [
	tag := aTag
]
