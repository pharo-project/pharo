"
I am a variable node for representing an indexed variable stored in a Context object that does not have a name. These are values that are pushed and popped on the stack when executing message sends. 
For example, the parameters needed to perform a message send are pushed in a Context object.
"
Class {
	#name : #StInspectorContextIndexedTempNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'tempIndex'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'as yet unclassified' }
StInspectorContextIndexedTempNode class >> hostObject: aContext tempIndex: anInteger [
	^ (self hostObject: aContext) 
		tempIndex: anInteger;
		yourself
]

{ #category : #accessing }
StInspectorContextIndexedTempNode >> label [
	^ self tempIndex
]

{ #category : #accessing }
StInspectorContextIndexedTempNode >> rawValue [

	^ self hostObject at: self tempIndex
]

{ #category : #accessing }
StInspectorContextIndexedTempNode >> tempIndex [
	^ tempIndex
]

{ #category : #accessing }
StInspectorContextIndexedTempNode >> tempIndex: anObject [
	tempIndex := anObject
]

{ #category : #accessing }
StInspectorContextIndexedTempNode >> variableTag [
	^  'temp' 
]
