"
I model an attribute of an object in the `StInspector`. I am used in the Raw presentation of an Object. My main responsibility is to associate a variable name of a host object with its value. 

Public API and Key Messages

- hostObject return the object (host) holding the  attribute that I represent;
- label return a string label of the attribute, which may be an instance variable name or a dynamically generated name for dynamic attributes;
- value return the value  of the attribute; catches any errors and returns nil;
- rawValue return the value of the attribute without any catch for errors;
- key return an object used as unique key to identify an attribute, it may be an integer for indexed variables or a string for dynamic ones.

"
Class {
	#name : #StInspectorNode,
	#superclass : #Object,
	#instVars : [
		'hostObject'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StInspectorNode class >> hostObject: anObject [ 
	^ self new
		hostObject: anObject;
		yourself
]

{ #category : #comparing }
StInspectorNode >> = anObject [

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ hostObject == anObject hostObject
]

{ #category : #converting }
StInspectorNode >> asInspectorModel [

	^ StInspectorModel on: self rawValue
]

{ #category : #accessing }
StInspectorNode >> children [ 
	
	^ self value allInspectorNodes
]

{ #category : #accessing }
StInspectorNode >> errorMessage [
	^ 'error obtaining value'
]

{ #category : #comparing }
StInspectorNode >> hash [

	^ hostObject hash
]

{ #category : #accessing }
StInspectorNode >> hostObject [
	^ hostObject
]

{ #category : #accessing }
StInspectorNode >> hostObject: anObject [

	"
	Allow nil objects for the moment. Normally the inspector should be able to inspect nil.
	self
		assert: [ anObject isNotNil ]
		description: [ 'Host object must not be nil' ]."

	self
		assert: [ hostObject isNil ]
		description: [ 'Host can be assigned only once' ].

	hostObject := anObject
]

{ #category : #accessing }
StInspectorNode >> icon [ 

	^ [ 
		(self value gtInspectorIcon) ifNil: [ 
			self value class systemIcon ] ]
	on: Error do: [ :error | 
		self iconError: error ]
]

{ #category : #private }
StInspectorNode >> iconError: error [
	
	^ self iconNamed: #smallWarningIcon
]

{ #category : #accessing }
StInspectorNode >> key [
	"Answer the key of this object variable (slot, indexed attribute, computed value)."
	^ self subclassResponsibility
]

{ #category : #accessing }
StInspectorNode >> label [
	"Answer the label of this object variable (slot, indexed attribute, computed value)."
	^ self key asString
]

{ #category : #accessing }
StInspectorNode >> mementoValue [

	^ self value
]

{ #category : #accessing }
StInspectorNode >> rawValue [
	"Answer the object value of this object variable (slot, indexed attribute, computed value)."
	^ self subclassResponsibility
]

{ #category : #actions }
StInspectorNode >> save: aValue [
]

{ #category : #accessing }
StInspectorNode >> stringValue [

	^ self value stDisplayString
]

{ #category : #accessing }
StInspectorNode >> value [
	"Answer the object value of this object variable (slot, indexed attribute, computed value).
	Return nil in case there is an error obtaining the value"
	^ [ self rawValue ] on: Error do: [ nil ]
]

{ #category : #accessing }
StInspectorNode >> variableTag [
	^ 'implicit'
]
