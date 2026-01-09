Class {
	#name : 'ReNameIsGlobalCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'className'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'accessing - private' }
ReNameIsGlobalCondition >> candidates [

	^ { className }
]

{ #category : 'instance creation' }
ReNameIsGlobalCondition >> model: aRBNamespace className: aSymbol [

	className := aSymbol asSymbol.
	model := aRBNamespace 
]

{ #category : 'displaying' }
ReNameIsGlobalCondition >> violationMessageOn: aStream [
	self deprecated: 'Use violationMessageOn:violators: instead'.
	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is <1?:not >a class or global variable.' ]
]

{ #category : 'displaying' }
ReNameIsGlobalCondition >> violationMessageOn: aStream violators: theViolators [

	| verb |
	theViolators ifEmpty: [ ^ self ].
	verb := self violators = theViolators
		        ifTrue: [ ' is not ' ]
		        ifFalse: [ ' is ' ].

	theViolators do: [ :violator |
			aStream
				nextPutAll: violator;
				nextPutAll: verb;
				nextPutAll: 'a class or global variable.' ]
]

{ #category : 'accessing' }
ReNameIsGlobalCondition >> violators [

	^ (model includesGlobal: className asSymbol) 
		ifTrue: [ #() ] 
		ifFalse: [ { className } ]
]
