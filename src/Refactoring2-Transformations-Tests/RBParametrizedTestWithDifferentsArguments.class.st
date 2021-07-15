Class {
	#name : #RBParametrizedTestWithDifferentsArguments,
	#superclass : #RBParametrizedTestWithDifferentConstructors,
	#instVars : [
		'index'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #builder }
RBParametrizedTestWithDifferentsArguments >> createRefactoringWithArguments: aParameterCollection [
	^ (rbClass
		perform: constructor 
		withArguments: (self selectIndex: index of: aParameterCollection)) asRefactoring
]

{ #category : #builder }
RBParametrizedTestWithDifferentsArguments >> createRefactoringWithModel: rbNamespace andArguments: aParameterCollection [
	^ (rbClass
		perform: #model: , constructor 
		withArguments: {rbNamespace}, 
			(self selectIndex: index of: aParameterCollection)) asRefactoring
]

{ #category : #accessing }
RBParametrizedTestWithDifferentsArguments >> index: aBlock [ 

	index := aBlock
]

{ #category : #accessing }
RBParametrizedTestWithDifferentsArguments >> selectIndex: anInteger of: aCollection [
	^ aCollection collect: [ :each | each isArray ifTrue: [each at: anInteger ] ifFalse: each ]
]
