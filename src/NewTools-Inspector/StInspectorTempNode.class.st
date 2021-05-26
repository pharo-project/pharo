"
I am a variable node for representing a temporary variable stored in a Context object that has a name attached. 
This includes  local variables and method parameters.
"
Class {
	#name : #StInspectorTempNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'tempVariable'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #comparing }
StInspectorTempNode >> = anObject [

	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ tempVariable = anObject tempVariable and: [ 
		  hostObject = anObject hostObject ]
]

{ #category : #accessing }
StInspectorTempNode >> children [
	^ #()
]

{ #category : #accessing }
StInspectorTempNode >> debuggerColor [
	^ tempVariable debuggerColors
]

{ #category : #comparing }
StInspectorTempNode >> hash [

	"Answer an integer value that is related to the identity of the receiver."

	^ tempVariable hash bitXor: (hostObject hash)
]

{ #category : #accessing }
StInspectorTempNode >> key [
	^ tempVariable name
]

{ #category : #accessing }
StInspectorTempNode >> rawValue [
	"Answer the object value of this object variable (slot, indexed attribute, computed value)."

	^ [ 
		"we use #tempNamed: to force a re-lookup of the variable"
		self hostObject tempNamed: tempVariable name]
		  on: Error
		  do: [ :err | 'cannot read ' , tempVariable name ]
]

{ #category : #accessing }
StInspectorTempNode >> tempVariable [
	^ tempVariable
]

{ #category : #accessing }
StInspectorTempNode >> tempVariable: anObject [
	tempVariable := anObject
]

{ #category : #accessing }
StInspectorTempNode >> variableTag [

	^ tempVariable variableTag
]
