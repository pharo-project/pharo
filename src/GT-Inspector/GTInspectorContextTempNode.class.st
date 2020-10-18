Class {
	#name : #GTInspectorContextTempNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'temp',
		'tag'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorContextTempNode class >> hostObject: aContext temp: aVar [
	^ (self hostObject: aContext) 
		temp: aVar;
		variableTag: ((aContext method argumentNames includes: aVar name)
			ifTrue: [ 'parameter' ]
			ifFalse: [ 'temp' ])
		yourself
]

{ #category : #accessing }
GTInspectorContextTempNode >> errorMessage [
	^ 'error obtaining field value'
]

{ #category : #accessing }
GTInspectorContextTempNode >> key [
	^ self tempName
]

{ #category : #accessing }
GTInspectorContextTempNode >> label [
	^ '[' , self key , ']'
]

{ #category : #accessing }
GTInspectorContextTempNode >> rawValue [
	^ temp readInContext: self hostObject
]

{ #category : #accessing }
GTInspectorContextTempNode >> temp [
	^ temp
]

{ #category : #accessing }
GTInspectorContextTempNode >> temp: anObject [
	temp := anObject
]

{ #category : #accessing }
GTInspectorContextTempNode >> tempName [
	^ temp name
]

{ #category : #accessing }
GTInspectorContextTempNode >> variableTag [
	^ tag ifNil: [ 'temp' ]
]

{ #category : #accessing }
GTInspectorContextTempNode >> variableTag: aTag [
	tag := aTag
]
