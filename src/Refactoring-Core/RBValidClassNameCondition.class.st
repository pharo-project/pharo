Class {
	#name : 'RBValidClassNameCondition',
	#superclass : 'RBCondition',
	#instVars : [
		'className'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RBValidClassNameCondition >> check [ 

	^ self class checkClassVarName: className in: self
]

{ #category : 'accessing' }
RBValidClassNameCondition >> className: aSymbol [

	className := aSymbol asSymbol 
]

{ #category : 'accessing' }
RBValidClassNameCondition >> violationMessageOn: aStream [

	self violators do: [ :violator |
		aStream
			nextPutAll: violator;
			nextPutAll: ' is <1?:not >a valid class name.' ]
]

{ #category : 'accessing' }
RBValidClassNameCondition >> violators [

	^ self check ifFalse: [ { className } ] ifTrue: [ #() ]
]
