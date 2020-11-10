"
I model self, thisContext and super
"
Class {
	#name : #ReservedVariable,
	#superclass : #Variable,
	#instVars : [
		'environment'
	],
	#classInstVars : [
		'instance'
	],
	#category : #'Kernel-Variables'
}

{ #category : #accessing }
ReservedVariable class >> instance [ 
	^instance ifNil: [ instance := self new ]
]

{ #category : #testing }
ReservedVariable class >> isAbstract [ 
	^self = ReservedVariable  
]

{ #category : #compiler }
ReservedVariable class >> lookupDictionary [
	"create a loopup table name -> instance for Semantic Analysis"
	^ (self subclasses collect: [ :class | 
		   class instance name -> class instance ]) asDictionary
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

{ #category : #initialization }
ReservedVariable >> initialize [
	environment := Smalltalk globals
]

{ #category : #testing }
ReservedVariable >> isReservedVariable [
	^true
]

{ #category : #testing }
ReservedVariable >> isWritable [
	^ false
]

{ #category : #accessing }
ReservedVariable >> name: aSymbol [
	"names of ReservedVariables are fixed and can not be changed"
	self shouldNotImplement
]

{ #category : #printing }
ReservedVariable >> printOn: stream [

	stream nextPutAll: self name
]

{ #category : #accessing }
ReservedVariable >> scope [
	^ environment
]

{ #category : #queries }
ReservedVariable >> usingMethods [
	"first call is very slow as it creates all ASTs"
	^environment allMethods select: [ : method |
		method ast variableNodes anySatisfy: [ :varNode | varNode variable == self]]
]

{ #category : #debugging }
ReservedVariable >> write: aValue inContext: aContext [
	
	self error: name, ' is reserved word and cant be modified'
]
