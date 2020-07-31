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

{ #category : #testing }
ReservedVariable class >> nameIsReserved: aName [
	^self subclasses anySatisfy: [ :class | class instance name = aName ]
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
ReservedVariable >> isReservedVariable [
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

{ #category : #queries }
ReservedVariable >> usingMethods [
	"first call is very slow as it creates all ASTs"
	^SystemNavigation new allMethods select: [ : method |
		method ast variableNodes anySatisfy: [ :varNode | varNode binding == self]]
]

{ #category : #debugging }
ReservedVariable >> write: aValue inContext: aContext [
	
	self error: name, ' is reserved word and cant be modified'
]
