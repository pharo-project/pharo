Class {
	#name : #RBCompositeTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBCompositeTransformationTest >> testTransform [

	| transformation newClassName class |
	newClassName := (self changeMock name, 'Temporary') asSymbol.
	transformation := RBCompositeTransformation new
		transformations: (OrderedCollection new
			add: (RBAddClassTransformation
					addClass: newClassName
					superclass: #Object
					subclasses: #() 
					category: self class category);
			add: (RBAddVariableTransformation 
					instanceVariable: 'asdf'
					class: newClassName);
			add: (RBAddMethodTransformation
					sourceCode: 'printString1 ^super printString'
					in: newClassName
					withProtocol: #accessing);
			yourself).
	transformation transform.
	
	self assert: transformation model changes changes size equals: 3.
	
	class := transformation model classNamed: (self changeMock name, 'Temporary') asSymbol.
	self assert: (class directlyDefinesInstanceVariable: 'asdf').
	self assert: (class parseTreeFor: #printString1)
		  equals: (self parseMethod: 'printString1 ^super printString')
]
