Class {
	#name : 'ReValidClassNameCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'className'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReValidClassNameCondition >> check [ 

	^ self class checkClassVarName: className in: self
]

{ #category : 'accessing' }
ReValidClassNameCondition >> className: aSymbol [

	className := aSymbol asSymbol 
]

{ #category : 'accessing' }
ReValidClassNameCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is <1?:not >a valid class name.' ]
]

{ #category : 'accessing' }
ReValidClassNameCondition >> violators [

	^ self check ifFalse: [ { className } ] ifTrue: [ #() ]
]
