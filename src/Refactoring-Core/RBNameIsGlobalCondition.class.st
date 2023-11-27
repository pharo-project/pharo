Class {
	#name : 'RBNameIsGlobalCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'className',
		'model'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBNameIsGlobalCondition >> check [  

	^ model includesGlobal: className 
]

{ #category : 'instance creation' }
RBNameIsGlobalCondition >> model: aRBNamespace className: aSymbol [

	className := aSymbol asSymbol.
	model := aRBNamespace 
]

{ #category : 'displaying' }
RBNameIsGlobalCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is <1?:not >a class or global variable.' ]
]

{ #category : 'accessing' }
RBNameIsGlobalCondition >> violators [

	^ self check ifFalse: [ { className } ] ifTrue: [ #() ]
]
