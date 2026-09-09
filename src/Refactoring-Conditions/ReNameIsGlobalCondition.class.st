Class {
	#name : 'ReNameIsGlobalCondition',
	#superclass : 'ReReifiedCondition',
	#instVars : [
		'className',
		'model'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'checking' }
ReNameIsGlobalCondition >> check [  

	^ model includesGlobal: className 
]

{ #category : 'instance creation' }
ReNameIsGlobalCondition >> model: aRBNamespace className: aSymbol [

	className := aSymbol asSymbol.
	model := aRBNamespace 
]

{ #category : 'displaying' }
ReNameIsGlobalCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is <1?:not >a class or global variable.' ]
]

{ #category : 'accessing' }
ReNameIsGlobalCondition >> violators [

	^ self check ifFalse: [ { className } ] ifTrue: [ #() ]
]
