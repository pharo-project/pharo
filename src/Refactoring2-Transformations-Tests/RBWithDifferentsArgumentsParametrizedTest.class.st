Class {
	#name : 'RBWithDifferentsArgumentsParametrizedTest',
	#superclass : 'RBWithDifferentConstructorsParametrizedTest',
	#instVars : [
		'index'
	],
	#category : 'Refactoring2-Transformations-Tests-Parametrized',
	#package : 'Refactoring2-Transformations-Tests',
	#tag : 'Parametrized'
}

{ #category : 'builder' }
RBWithDifferentsArgumentsParametrizedTest >> createRefactoringWithArguments: aParameterCollection [

	^ rbClass
		  perform: constructor
		  withArguments: (self selectIndex: index of: aParameterCollection)
]

{ #category : 'builder' }
RBWithDifferentsArgumentsParametrizedTest >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [

	^ rbClass
		  perform: #model: , constructor
		  withArguments:
		  { rbNamespace }
		  , (self selectIndex: index of: aParameterCollection)
]

{ #category : 'accessing' }
RBWithDifferentsArgumentsParametrizedTest >> index: aBlock [

	index := aBlock
]

{ #category : 'accessing' }
RBWithDifferentsArgumentsParametrizedTest >> selectIndex: anInteger of: aCollection [
	^ aCollection collect: [ :each | each isArray ifTrue: [each at: anInteger ] ifFalse: each ]
]
