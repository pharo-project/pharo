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

{ #category : #accessing }
StInspectorTempNode >> key [
	^ tempVariable name
]

{ #category : #accessing }
StInspectorTempNode >> rawValue [
	"Answer the object value of this object variable (slot, indexed attribute, computed value)."

	^ [ tempVariable readInContext: self hostObject ]
		  on: SubscriptOutOfBounds
		  do: [ :err | 'cannot read ' , tempVariable name ]
]

{ #category : #accessing }
StInspectorTempNode >> tempVariable: anObject [
	tempVariable := anObject
]
