Class {
	#name : #RBAddPragmaTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #utilities }
RBAddPragmaTransformationTest >> methodAfter [
	<gtInspectorPresentationOrder: 56>

	| variable |
	variable := 'String'.
]

{ #category : #utilities }
RBAddPragmaTransformationTest >> methodBefore [

	| variable |
	variable := 'String'.
]

{ #category : #tests }
RBAddPragmaTransformationTest >> testClassDoesNotExist [

	self shouldFail: (RBAddPragmaTransformation
							pragma: '<gtInspectorPresentationOrder: 56>'
							inMethod: #methodBefore
							inClass: #RBPragmaTransformationTest)
							asRefactoring 
]

{ #category : #tests }
RBAddPragmaTransformationTest >> testMethodDoesNotExist [

	self shouldFail: (RBAddPragmaTransformation
							pragma: '<gtInspectorPresentationOrder: 56>'
							inMethod: #method
							inClass: #RBAddPragmaTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddPragmaTransformationTest >> testPragmaAlreadyExists [

	self shouldFail: (RBAddPragmaTransformation
							pragma: '<gtInspectorPresentationOrder: 56>'
							inMethod: #methodAfter
							inClass: #RBAddPragmaTransformationTest)
							asRefactoring
]

{ #category : #tests }
RBAddPragmaTransformationTest >> testRefactoring [

	| refactoring class |
	refactoring := (RBAddPragmaTransformation
						pragma: '<gtInspectorPresentationOrder: 56>'
						inMethod: #methodBefore
						inClass: #RBAddPragmaTransformationTest)
						asRefactoring transform.
	
	self assert: refactoring model changes changes size equals: 1.
	
	class := refactoring model classNamed: #RBAddPragmaTransformationTest.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]

{ #category : #tests }
RBAddPragmaTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddPragmaTransformation new
		pragma: '<gtInspectorPresentationOrder: 56>'
		inMethod: #methodBefore
		inClass: self class name)
		transform.
	
	self assert: transformation model changes changes size equals: 1.
	
	class := transformation model classNamed: self class name.
	self assert: (class parseTreeFor: #methodBefore) body
			equals: (class parseTreeFor: #methodAfter) body.
]
