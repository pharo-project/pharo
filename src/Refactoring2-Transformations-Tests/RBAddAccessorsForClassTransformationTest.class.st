Class {
	#name : #RBAddAccessorsForClassTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBAddAccessorsForClassTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddAccessorsForClassTransformation className: #RBVariableTransformation) asRefactoring transform.

	self assert: refactoring model changes changes size equals: 4.

	class := refactoring model classNamed: #RBVariableTransformation.
	self assert: (class parseTreeForSelector: #variableName)
		  equals: (self parseMethod: 'variableName ^variableName').
	self assert: (class parseTreeForSelector: #variableName:)
		  equals: (self parseMethod: 'variableName: anObject variableName := anObject')
]

{ #category : #tests }
RBAddAccessorsForClassTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddAccessorsForClassTransformation className: self changeMock name) transform.

	self assert: transformation model changes changes size equals: 2.

	class := transformation model classNamed: self changeMock name asSymbol.
	self assert: (class parseTreeForSelector: #instVar)
		  equals: (self parseMethod: 'instVar ^instVar').
	self assert: (class parseTreeForSelector: #instVar:)
		  equals: (self parseMethod: 'instVar: anObject instVar := anObject')
]
