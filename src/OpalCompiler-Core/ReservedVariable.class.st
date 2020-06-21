"
I model self, thisContext and super
"
Class {
	#name : #ReservedVariable,
	#superclass : #Variable,
	#classInstVars : [
		'instance'
	],
	#category : #'OpalCompiler-Core-Semantics'
}

{ #category : #accessing }
ReservedVariable class >> instance [ 
	^instance ifNil: [ instance := self new ]
]

{ #category : #testing }
ReservedVariable class >> isAbstract [ 
	^self = ReservedVariable  
]

{ #category : #converting }
ReservedVariable >> asString [

	^ self name
]

{ #category : #'code generation' }
ReservedVariable >> emitStore: methodBuilder [

	self shouldNotImplement
]

{ #category : #testing }
ReservedVariable >> isSpecialVariable [
	^true
]

{ #category : #testing }
ReservedVariable >> isWritable [
	^ false
]

{ #category : #printing }
ReservedVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #debugging }
ReservedVariable >> write: aValue inContext: aContext [
	
	self error: name, ' is reserved word and cant be modified'
]
