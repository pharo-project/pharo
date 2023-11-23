"
I am a mock class used in the tests to get the information.
"
Class {
	#name : 'CoMockClass',
	#superclass : 'Object',
	#instVars : [
		'instanceVariables',
		'classVariables',
		'superclass',
		'methodDictionary',
		'sharedPools'
	],
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'configuration' }
CoMockClass >> addInstanceVariable: aString [

	instanceVariables := instanceVariables copyWith: aString
]

{ #category : 'accessing' }
CoMockClass >> allClassVarNames [
	"Answer the names of the receiver's and the receiver's ancestor's class variables."

	^ self superclass
		ifNil: [ self classVarNames ]
		ifNotNil: [ :sup | self classVarNames, sup allClassVarNames ]
]

{ #category : 'accessing' }
CoMockClass >> allInstVarNames [

	| superclassVarNames |
	superclassVarNames := superclass
		ifNil: [ #() ]
		ifNotNil: [ superclass allInstVarNames ].
	^ superclassVarNames , instanceVariables
]

{ #category : 'accessing' }
CoMockClass >> classVarNames [

	^ classVariables
]

{ #category : 'accessing' }
CoMockClass >> classVariables: aCollection [

	classVariables := aCollection
]

{ #category : 'initialization' }
CoMockClass >> initialize [

	super initialize.
	methodDictionary := OrderedDictionary new
]

{ #category : 'accessing' }
CoMockClass >> instVarNames [

	^ self instanceVariables
]

{ #category : 'accessing' }
CoMockClass >> instanceSide [

	^ self
]

{ #category : 'accessing' }
CoMockClass >> instanceVariables [
	^ instanceVariables
]

{ #category : 'accessing' }
CoMockClass >> instanceVariables: anObject [
	instanceVariables := anObject
]

{ #category : 'testing' }
CoMockClass >> isTestCase [

	^ false
]

{ #category : 'methods' }
CoMockClass >> lookupSelector: aString [

	^ methodDictionary
		at: aString
		ifAbsent: [
			superclass ifNil: [ ^ nil ].
			superclass lookupSelector: aString ]
]

{ #category : 'methods' }
CoMockClass >> methodAt: aString put: aCoMockMethod [

	methodDictionary at: aString asSymbol put: aCoMockMethod.
	aCoMockMethod methodClass: self
]

{ #category : 'accessing' }
CoMockClass >> selectors [
	^ methodDictionary keys
]

{ #category : 'accessing' }
CoMockClass >> selectors: aCollection [

	"Install nil methods just for compatibility"
	aCollection do: [ :k |
		methodDictionary at: k put: nil
	]
]

{ #category : 'enumerating' }
CoMockClass >> selectorsDo: aBlockClosure [

	self selectors do: aBlockClosure
]

{ #category : 'accessing' }
CoMockClass >> sharedPools [

	^ sharedPools ifNil: [ #() ]
]

{ #category : 'accessing' }
CoMockClass >> sharedPools: aCollection [
	sharedPools := aCollection
]

{ #category : 'accessing' }
CoMockClass >> superclass [

	^ superclass
]

{ #category : 'accessing' }
CoMockClass >> superclass: aCompletionMockClass [

	superclass := aCompletionMockClass
]
