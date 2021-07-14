Class {
	#name : #RBAddMethodTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #running }
RBAddMethodTransformationTest >> setUp [

	super setUp.
	model := self abstractVariableTestData.
]

{ #category : #tests }
RBAddMethodTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
		sourceCode: 'printString1 ^super printString'
		in: self changeMock name
		withProtocols: {#accessing})
		transform.
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]
